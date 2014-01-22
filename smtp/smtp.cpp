#include "smtp/smtp.h"
#include "smtp/MIME-conformance/Mime.h"
#include "smtp/MIME-conformance/MimeCode.h"
#include "smtp/HtmlParser.h"

#include "base/logging.h"
#include "base/sslsocketfactory.h"
#include "base/win32socket.h"
#include "base/socketaddress.h"
#include "base/base64.h"
#include "base/common.h"
#include "base/stringutils.h"

#define SMTP_START	1000
#define SMTP_FINAL	1001
#define SMTP_FAILD	1002

Smtp::Smtp(const char* hostname,int port,
			const char* username,const char* userpassword,
			Thread* main /*= NULL*/)
:hostname_(hostname)
,username_(username)
,userpassword_(userpassword)
,state_(SMTP_NONE)
,port_(port)
,Index_(0)
,Recipient_index_(0)
,CCRecipient_index_(0)
,BCCRecipient_index_(0)
,socket_(NULL)
,bSsl_(false)
,bIgnoreBadCert_(false)
,main_((main == NULL) ? Thread::Current() : main)
,worker_(new Thread){

	Recipients_.clear();
	CCRecipients_.clear();
	BCCRecipients_.clear();
	Attachments_.clear();

}

Smtp::~Smtp() {
	delete worker_;
	delete socket_;
}

void Smtp::OnMessage(Message* msg) {
	if (msg->message_id == SMTP_START) {
		ASSERT(worker_->IsCurrent());
		StartSmtp();
	} else if (msg->message_id == SMTP_FINAL) {
		ASSERT(main_->IsCurrent());
		SuccessfulEvent();
		worker_->Stop();
	} else if (msg->message_id == SMTP_FAILD) {
		ASSERT(main_->IsCurrent());
		ErrorEvent("socket error");
	}
}

int Smtp::StartSmtp() {
	//Win32SocketServer ss(NULL);
	SocketServer* ss = worker_->socketserver();
	SslSocketFactory socket_factory(ss);

	socket_factory.SetIgnoreBadCert(bIgnoreBadCert_);
	if (bSsl_) {
		socket_factory.UseSSL(hostname_.c_str());
	}

	socket_ = socket_factory.CreateAsyncSocket(AF_INET,SOCK_STREAM);
	if (!socket_)
		return 1;

	SocketAddress addr(hostname_,port_);
	if (socket_->Connect(addr) == SOCKET_ERROR)
		return 1;

	socket_->SignalConnectEvent.connect(this,&Smtp::OnConnectEvent);
	socket_->SignalReadEvent.connect(this,&Smtp::OnReadEvent);
	socket_->SignalWriteEvent.connect(this,&Smtp::OnWriteEvent);
	socket_->SignalCloseEvent.connect(this,&Smtp::OnCloseEvent);
	
	return 0;
}

int Smtp::Go() {
	state_ = SMTP_NONE;
	ASSERT(state_ == SMTP_NONE);

	if (Recipients_.empty())
		return 1;

	if (!worker_->Start())
		return 1;

	worker_->Post(this,SMTP_START);
	return 0;
}

void Smtp::Final() {
	main_->Quit();
}

int Smtp::AddRecipient(const char*mail,const char* name) {
	return AddUserTo(Recipients_,mail,name);
}

int Smtp::RemoveRecipient(const char*mail,const char* name) {
	return RemoveUserFrom(Recipients_,mail,name);
}

int Smtp::AddCCRecipient(const char*mail,const char* name) {
	return AddUserTo(CCRecipients_,mail,name);
}

int Smtp::RemoveCCRecipient(const char*mail,const char* name) {
	return RemoveUserFrom(CCRecipients_,mail,name);
}

int Smtp::AddBCCRecipient(const char*mail,const char* name) {
	return AddUserTo(BCCRecipients_,mail,name);
}

int Smtp::RemoveBCCRecipient(const char*mail,const char* name) {
	return RemoveUserFrom(BCCRecipients_,mail,name);
}

int Smtp::AddUserTo(USERLIST &list,const char*mail,const char* name) {
	if (mail == NULL)
		return -1;

	std::string str_mail(mail);

	if (str_mail.find("@") == string::npos)
		return -1;

	Recipient recipient;
	recipient.mail = str_mail;
	recipient.name = (name == NULL) ? "" : name;

	list.push_back(recipient);
	return list.size();
}

int Smtp::RemoveUserFrom(USERLIST &list,const char*mail,const char* name) {
	if (mail == NULL)
		return -1;

	std::string str_mail(mail);
	if (str_mail.find("@") == string::npos)
		return -1;
	
	if (list.empty())
		return -1;

	USERLIST::iterator it = list.begin();
	for(; it != list.end(); it++) {
		const Recipient &recipient = *it;
		if (recipient.mail == str_mail) {
			list.erase(it);
			break;
		}
	}

	return list.size();
}

void Smtp::OnConnectEvent(AsyncSocket* socket) {
	LOG(INFO) << "Smtp OnConnectEvent";
}

void Smtp::OnReadEvent(AsyncSocket* socket) {
	char buffer[4096] = {0};
	int read = socket->Recv(buffer,4096);
	if (read == SOCKET_ERROR) {
		int error = socket->GetError();
		if (error != EWOULDBLOCK)
			OnCloseEvent(socket,error);
	}

	if (read > 0) {
		//LOG(INFO) << "Smtp OnReadEvent";
		LOG(LS_VERBOSE) << buffer ;

		int ret = 0;
		if (ret = HandleData(buffer,read) != 0) {
			//OnCloseEvent(socket,ret);
			ErrorEvent(std::string(buffer));
		}
	}
}

void Smtp::OnWriteEvent(AsyncSocket* socket) {
	LOG(INFO) << "Smtp OnWriteEvent (" << socket->GetError() << ")";
}

void Smtp::OnCloseEvent(AsyncSocket* socket, int err) {
	LOG(INFO) << "Smtp OnCloseEvent << (" << err << ")";
	// 我们的smtp服务存在一个异常问题：
	// 异步socket发送完大一点的数据后，
	// smtp服务不会返回250 ok的响应包
	// 这里在socket层做了一个超时处理，超时后
	// 我们主动断开socket，接收方能收到发送出去的邮件
	if (err != 0 && err != WSAETIMEDOUT) {
		//ErrorEvent("socket error"); //process in mainthread
		Notify(1);
	}

	Clean();

	if (err == WSAETIMEDOUT && state_ == SMTP_DATA) {
		state_ = SMTP_QUIT;
		Notify(0);
	}
}

int Smtp::SmtpXYZdigits(const char* data) {
	if (data == NULL) {
		return -1;
	}
	
	char ret_code[4] = {0};
	memcpy(ret_code,data,3);
	return (::atoi(ret_code));
}

void Smtp::Clean() {
	//state_ = SMTP_NONE;
	socket_->Close();
}

int Smtp::HandleData(const char* data,size_t size) {
	int ret = SmtpXYZdigits(data);
	switch (state_) {
	case SMTP_NONE:
		if (ret == SMTP_SERVICE_READ) {
			state_ = SMTP_CONNECTED;
			ret = 0;
			ehlo_command();
		}
		break;
	case SMTP_CONNECTED:
		if (ret == SMTP_REQUEST_COMPLETE) {
			state_ = SMTP_EHLO;
			ret = 0;
			auth_command();
		}
		break;
	case SMTP_EHLO: 
		if (ret == SMTP_RESPONSE_AUTH_BASE64) {
			state_ = SMTP_AUTH;
			ret = 0;
			send_username();
		}
		break;
	case SMTP_AUTH:
		if (ret == SMTP_RESPONSE_AUTH_BASE64) {
			state_ = SMTP_USERNAME;
			ret = 0;
			send_password();
		}
		break;
	case SMTP_USERNAME:
		if (ret == SMTP_AUTH_SUCCESS) {
			state_ = SMTP_PASSWORD;
			ret = 0;
			mail_from_command();
		}
		break;
	case SMTP_PASSWORD:
		if (ret == SMTP_REQUEST_COMPLETE) {
			state_ = SMTP_SENT_FROM;
			ret = 0;
			rcpt_command();
		}
		break;
	case SMTP_SENT_FROM:
		if (ret == SMTP_REQUEST_COMPLETE) {
			if (!IsDoneRcpt()) {
				state_ = SMTP_SENT_FROM;
				ret = 0;
				rcpt_command();
			} else {
				state_ = SMTP_RCPT;
				ret = 0;
				data_command();
			}
		}
		break;
	case SMTP_RCPT:
		if (ret == SMTP_BEGIN_INPUT_MAIL) {
			state_ = SMTP_DATA;
			ret = 0;
			send_body();
		}
		break;
	case SMTP_DATA:
		if (ret == SMTP_REQUEST_COMPLETE) {
			// should quit
			state_ = SMTP_BODY;
			ret = 0;
			quit_command();
		}
		break;
	case SMTP_BODY:
		if (ret == SMTP_SERVICE_CLOSE) {
			state_ = SMTP_QUIT;
			ret = 0;
			Clean();

			Notify(0);
		}
		break;
	default:
		LOG(LS_ERROR) << "handle smtp error (" << state_ << ")";
		return SMTP_UNKOWN;
	}
	return ret;
}
int Smtp::ehlo_command() {
	ASSERT(state_ == SMTP_CONNECTED);
	char ehlo[1024] = {0};
	char hostname[256] = {0};
	gethostname(hostname,255);
	_snprintf_s(ehlo,1024,"EHLO %s\r\n",hostname);
	
	LOG(LS_VERBOSE) << "request >> " << ehlo ;

	return socket_->Send(ehlo,strlen(ehlo));
}

int Smtp::auth_command() {
	ASSERT(state_ == SMTP_EHLO);
	const char* auth = "AUTH LOGIN\r\n";

	LOG(LS_VERBOSE) << "request >> "  << auth ;

	return socket_->Send(auth,strlen(auth));
}

int Smtp::mail_from_command() {
	ASSERT(state_ == SMTP_PASSWORD);
	char from[1024] = {0};
	_snprintf_s(from,1024,"MAIL FROM:<%s>\r\n",username_.c_str());

	LOG(LS_VERBOSE) << "request >> "  << from ;

	return socket_->Send(from,strlen(from));
}
int Smtp::send_username() {
	ASSERT(state_ == SMTP_AUTH);
	char username[1024] = {0};
	_snprintf_s(username,1024,"%s\r\n",Base64::Encode(username_).c_str());

	LOG(LS_VERBOSE) << "request >> "  << username ;

	return socket_->Send(username,strlen(username));
}

int Smtp::send_password() {
	ASSERT(state_ == SMTP_USERNAME);
	char password[1024] = {0};
	_snprintf_s(password,1024,"%s\r\n",Base64::Encode(userpassword_).c_str());

	LOG(LS_VERBOSE) << "request >> **********" ;

	return socket_->Send(password,strlen(password));
}

int Smtp::rcpt_command() {
	//ASSERT(state_ == SMTP_SENT_FROM);

	std::string str_rcpt;

	if (Recipient_index_ < Recipients_.size()) {
		const Recipient &recipient = Recipients_[Recipient_index_++];
		str_rcpt = "RCPT TO:<";
		str_rcpt += recipient.mail;
		str_rcpt += ">\r\n";

		Index_ ++;

		LOG(LS_VERBOSE) << "request >> "  << str_rcpt ;
		return socket_->Send(str_rcpt.c_str(),str_rcpt.size());
	}

	if (CCRecipient_index_ < CCRecipients_.size()) {
		const Recipient &recipient = CCRecipients_[CCRecipient_index_++];
		str_rcpt = "RCPT TO:<";
		str_rcpt += recipient.mail;
		str_rcpt += ">\r\n";
		
		Index_ ++;

		LOG(LS_VERBOSE) << "request >> "  << str_rcpt ;
		return socket_->Send(str_rcpt.c_str(),str_rcpt.size());
	}

	if (BCCRecipient_index_ < BCCRecipients_.size()) {
		const Recipient &recipient = BCCRecipients_[BCCRecipient_index_++];
		str_rcpt = "RCPT TO:<";
		str_rcpt += recipient.mail;
		str_rcpt += ">\r\n";
		
		Index_ ++;

		LOG(LS_VERBOSE) << "request >> "  << str_rcpt ;
		return socket_->Send(str_rcpt.c_str(),str_rcpt.size());
	}

	return 0;
}

int Smtp::data_command() {
	ASSERT(state_ == SMTP_RCPT);
	char *str = "data\r\n";

	LOG(LS_VERBOSE) << "request >> "  << str ;

	int sent = socket_->Send(str,strlen(str));
	ASSERT(sent == strlen(str));
	return sent;
}

int Smtp::MakeMailBody(std::string &body) {

	std::string Recipients,CCRecipients,BCCRecipients;
	if(!Recipients_.empty()) {
		for(size_t i = 0; i < Recipients_.size(); i++) {
			if (i > 0)
				Recipients += ",";
			Recipients += Recipients_[i].name;
			Recipients += "<";
			Recipients += Recipients_[i].mail;
			Recipients += ">";
		}
	}

	if(!CCRecipients_.empty()) {
		for(size_t i = 0; i < CCRecipients_.size(); i++) {
			if (i > 0)
				CCRecipients += ",";
			CCRecipients += CCRecipients_[i].name;
			CCRecipients += "<";
			CCRecipients += CCRecipients_[i].mail;
			CCRecipients += ">";
		}
	}

	if(!BCCRecipients_.empty()) {
		for(size_t i = 0; i < BCCRecipients_.size(); i++) {
			if (i > 0)
				BCCRecipients += ",";
			BCCRecipients += BCCRecipients_[i].name;
			BCCRecipients += "<";
			BCCRecipients += BCCRecipients_[i].mail;
			BCCRecipients += ">";
		}
	}

	bool bHasAttachments = !Attachments_.empty();
	std::vector<std::string> imgs;
	int count = CHtmlParser::LocalImg2Cid(messagebody_,imgs);
	
	CMimeMessage mail;
	mail.SetDate();
	mail.SetVersion();
	mail.SetFrom(username_.c_str(),NULL);
	if (!Recipients.empty())
		mail.SetTo(Recipients.c_str(),NULL);
	if (!CCRecipients.empty())
		mail.SetCc(CCRecipients.c_str(),NULL);
	if (!BCCRecipients.empty())
		mail.SetBcc(BCCRecipients.c_str());
	mail.SetSubject(subject_.c_str(),NULL);
	mail.SetContentType((!bHasAttachments && count > 0) ? 
						"multipart/related" : "multipart/mixed");
	mail.SetBoundary(NULL);

	CMimeBody* pBp = mail.CreatePart();
	if (count > 0) {
		pBp->SetContentType(bHasAttachments ? "multipart/related" : "multipart/alternative");
		pBp->SetBoundary("embeded_multipart_boundary");
		CMimeBody *pBpChild = pBp->CreatePart();

		if (bHasAttachments) {
			pBpChild->SetContentType("multipart/alternative");
			pBpChild->SetBoundary("sub_multipart_boundary");
			CMimeBody *pBpSub = pBpChild->CreatePart();
			pBpSub->SetContentType("text/html");
			pBpSub->SetTransferEncoding("base64");
			pBpSub->SetText(messagebody_.c_str());

			for ( vector<string>::iterator it = imgs.begin(); 
				it != imgs.end();
				it++) 
			{
				pBpChild = pBp->CreatePart();
				pBpChild->SetTransferEncoding("base64");
				pBpChild->ReadFromFile((*it).c_str());
				pBpChild->SetFieldValue("Content-ID", ("<" + pBpChild->GetName() + ">").c_str());
			}
		}
		else {
			pBpChild->SetContentType("text/html");
			pBpChild->SetTransferEncoding("base64");
			pBpChild->SetText(messagebody_.c_str());

			for ( vector<string>::iterator it = imgs.begin(); 
				it != imgs.end();
				it++) 
			{
				pBp = mail.CreatePart();
				pBp->SetTransferEncoding("base64");
				pBp->ReadFromFile((*it).c_str());
				pBp->SetFieldValue("Content-ID", ("<" + pBp->GetName() + ">").c_str());
			}
		}
	}
	else {

		pBp->SetContentType("text/html");
		pBp->SetTransferEncoding("base64");
		pBp->SetText(messagebody_.c_str());
	}

	// Add attachments body part
	for ( vector<string>::iterator it = Attachments_.begin(); 
		it != Attachments_.end();
		it++) 
	{
		pBp = mail.CreatePart();
		pBp->SetTransferEncoding("base64");
		pBp->ReadFromFile((*it).c_str());
	}

	int len = mail.GetLength();
	char *pBuff = (char*)malloc(len);
	mail.Store(pBuff, len);
	body = pBuff;
	body += "\r\n.\r\n";

	return body.size();
}

int Smtp::send_body() {
	std::string body;
	MakeMailBody(body);
	//int sent = socket_->Send(body.c_str(),body.size());

	int idx = 0,res,nLeft = body.size();

	while(nLeft > 0)
	{
		res = socket_->Send(&body[idx],nLeft);
		if(res == SOCKET_ERROR) {
			if (socket_->GetError() == WSAEWOULDBLOCK) {
				Sleep(100);	//
				continue;
			}
			ASSERT(0);
			return SOCKET_ERROR;
		}
		if(!res)
			break;
		nLeft -= res;
		idx += res;
	}
	ASSERT(idx == body.size());
	LOG(INFO) << "send body compeletedly";
	return idx;
}

int Smtp::quit_command() {
	ASSERT(state_ == SMTP_BODY);
	const char* quit = "QUIT\r\n";

	return socket_->Send(quit,strlen(quit));
}

void Smtp::Notify(int notify) {
	if (notify == 0) {
		//SuccessfulEvent();	//process in mainthread
		main_->Post(this,SMTP_FINAL);
	} else {
		main_->Post(this,SMTP_FAILD);
	}
}
