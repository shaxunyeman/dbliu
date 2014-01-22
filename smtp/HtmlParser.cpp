// HtmlParser.cpp: implementation of the CHtmlParser class.
//

#include "smtp/HtmlParser.h"

CHtmlParser::CHtmlParser()
{

}

CHtmlParser::~CHtmlParser()
{

}

int CHtmlParser::LocalImg2Cid(string& body, LocalImg& Imgs)
{
	string::size_type i, si, _si;
	int cnt = 0;

	i = body.find("<IMG");
	while (i != string::npos)
	{
		si = body.find("src=\"", i);
		si += 5;
		// ÅÐ¶ÏÊÇ·ñ"http://"
		if (si == body.find("http", si)) {
			i = body.find("<IMG", si);
			continue;
		}

		_si = body.find('"', si);

		string src = body.substr(si, _si-si);
		if (!src.empty()) {
			if (find(Imgs.begin(), Imgs.end(), src) == Imgs.end()) {
				Imgs.push_back(src);
				cnt++;
			}
			string cid = "cid:" + src.substr(src.rfind('\\')+1);
			body.replace(si, src.size(), cid);
			si += cid.size() + 2;
		}

		i = body.find("<IMG", si);
	}

	return cnt;
}
