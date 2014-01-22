#ifndef __H_NETAPP_SMTP_H__
#define __H_NETAPP_SMTP_H__

#include <string>
#include <vector>

#include "base/asyncsocket.h"
#include "base/sigslot.h"
#include "base/thread.h"
#include "base/messagehandler.h"
#include "base/messagequeue.h"
using namespace base;


class Smtp : public sigslot::has_slots<>
			,public MessageHandler {
public:
	Smtp(const char* hostname,int port,
			const char* username,const char* userpassword,
			Thread* main = NULL);
	~Smtp();

	sigslot::signal1<std::string> ErrorEvent;
	sigslot::signal0<> SuccessfulEvent;

	// Enable/Disable SSL
	void UseSsl(bool bSsl) {
		bSsl_ = bSsl;
	}

	// whether use ssl
	bool IsUseSsl() const {
		return bSsl_;
	}

	//whether ignore when cert is bad
	void SetIgnoreBadCert(bool ignore) {
		bIgnoreBadCert_ = ignore;
	}

	bool IsIgnoreBadCert() const {
		return bIgnoreBadCert_;
	}

	// start send mail using smtp
	int Go();
	// whether sending mail is successful
	bool IsSuccessful() const {
		return state_ == SMTP_QUIT;
	}

	// only using in console application when testing smtp.
	// use to stop main thread
	void Final();

	//set body of a mail
	void SetMessageBody(const char* messagebody) {
		messagebody_ = messagebody;
	}

	// set subject of a mail
	void SetSubject(const char* subject) {
		subject_ = subject;
	}

	// set attachments of a mail
	int AddAttachment(const char* attachment) {
		if (attachment == NULL)
			return -1;

		Attachments_.push_back(attachment);
		return Attachments_.size();
	}

	// add a recipient of a mail
	int AddRecipient(const char*mail,const char* name);
	int RemoveRecipient(const char*mail,const char* name);

	// add a ccrecipient of a mail
	int AddCCRecipient(const char*mail,const char* name);
	int RemoveCCRecipient(const char*mail,const char* name);

	// add a bccrecipient of a mail
	int AddBCCRecipient(const char*mail,const char* name);
	int RemoveBCCRecipient(const char*mail,const char* name);
protected:
	void OnConnectEvent(AsyncSocket* socket);
	void OnReadEvent(AsyncSocket* socket);
	void OnWriteEvent(AsyncSocket* socket);
	void OnCloseEvent(AsyncSocket* socket, int err);

	// messagehander
	virtual void OnMessage(Message* msg);
private:
	int StartSmtp();

	void Notify(int notify);
	int SmtpXYZdigits(const char* data);
	void Clean();
	//�ɹ����� 0�����򷵻ش�����һ����״̬
	int HandleData(const char* data,size_t size);
	int MakeMailBody(std::string &body);

	enum SmtpResponseCode {
		SMTP_SERVICE_READ = 220,			//�������
		SMTP_REQUEST_COMPLETE = 250,		//Ҫ����ʼ��������
		SMTP_NLOCAL_USER_FORWARD = 251,		//�û��Ǳ��أ���ת����<forward-path>
		SMTP_BEGIN_INPUT_MAIL = 354,		//��ʼ�ʼ����룬��<CRLF>.<CRLF>����
		SMTP_SERVICE_CLOSE = 221,			//����رմ����ŵ�
		SMTP_RESPONSE_AUTH_BASE64 = 334,	//��������Ӧ��֤Base64�ַ���
		SMTP_AUTH_SUCCESS = 235,			//��֤�ɹ�
		SMTP_ADDRESS_ERROR = 500,			//�����ַ����
		SMTP_PARAM_ERROR = 501,				//������ʽ����
		SMTP_NIMPLEMENT_COMMAND = 502,		//�����ʵ��
		SMTP_SERVICE_NEED_AUTH = 503,		//��������ҪSMTP��֤
		SMTP_NIMPLEMENT_PARAM_COMMAND = 504,//�����������ʵ��
		SMTP_SERVICE_NREAD = 421,			//����δ�������رմ����ŵ�
		SMTP_SERVICE_NUSE1 = 450,			//Ҫ����ʼ�����δ��ɣ����䲻���ã����磬����æ��
		SMTP_SERVICE_NUSE2 = 550,			//Ҫ����ʼ�����δ��ɣ����䲻���ã����磬����δ�ҵ����򲻿ɷ��ʣ�
		SMTP_DISCARD_REQUEST = 451,			//����Ҫ��Ĳ�������������г���
		SMTP_NLOCAL_USER_TRY = 551,			//�û��Ǳ��أ��볢��<forward-path>
		SMTP_NMEMORY = 452,					//ϵͳ�洢���㣬Ҫ��Ĳ���δִ��
		SMTP_OVER_MEMORY = 552,				//�����Ĵ洢���䣬Ҫ��Ĳ���δִ��
		SMTP_USER_INVALID = 553,			//�����������ã�Ҫ��Ĳ���δִ�У����������ʽ����
		SMTP_REQUEST_SWITCH_PWD = 432,		//��Ҫһ������ת��
		SMTP_AUTHMECHANISM_SIMPLE = 534,	//��֤���ƹ��ڼ�
		SMTP_CUR_AUTH_NEED_ENCRYPT = 538,	//��ǰ�������֤������Ҫ����
		SMTP_TEMP_AUTH_FAILED = 454,		//��ʱ��֤ʧ��
		SMTP_NEED_AUTH = 530,				//��Ҫ��֤"
		SMTP_UNKOWN	= 1024					//δ֪����
	};

	//smtp command
	int ehlo_command();
	int auth_command();
	int send_username();
	int send_password();
	int mail_from_command();
	int rcpt_command();
	bool IsDoneRcpt() const {
		if (Index_ == (Recipients_.size() + CCRecipients_.size() + BCCRecipients_.size()))
			return true;
		return false;
	}
	int data_command();
	int quit_command();
	int send_body();

	enum SmtpState {
		SMTP_NONE,
		SMTP_CONNECTED,
		SMTP_EHLO,
		SMTP_AUTH,
		SMTP_USERNAME,
		SMTP_PASSWORD,
		SMTP_SENT_FROM,
		SMTP_RCPT,
		SMTP_DATA,
		SMTP_BODY,
		SMTP_QUIT,
		SMTP_ERROR
	};
	int state_;
	std::string hostname_;
	std::string username_;
	std::string userpassword_;
	std::string messagebody_;
	std::string subject_;
	int port_;
	AsyncSocket* socket_;

	bool bSsl_;
	bool bIgnoreBadCert_;

	struct Recipient {
		std::string mail;
		std::string name;
	};
	typedef std::vector<Recipient> USERLIST;
	int AddUserTo(USERLIST &list,const char*mail,const char* name);
	int RemoveUserFrom(USERLIST &list,const char*mail,const char* name);

	USERLIST Recipients_;
	USERLIST CCRecipients_;
	USERLIST BCCRecipients_;
	size_t Index_;
	size_t Recipient_index_;
	size_t CCRecipient_index_;
	size_t BCCRecipient_index_;

	std::vector<std::string> Attachments_;

	Thread* main_;
	Thread* worker_;
};

#endif	//__H_NETAPP_SMTP_H__