// HtmlParser.h: interface for the CHtmlParser class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_HTMLPARSER_H__E5AE80A6_480D_4F29_A242_43AE16C4D11E__INCLUDED_)
#define AFX_HTMLPARSER_H__E5AE80A6_480D_4F29_A242_43AE16C4D11E__INCLUDED_

#include <vector>
#include <string>
#include <algorithm>
using std::vector;
using std::string;
using std::find;

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CHtmlParser  
{
public:
	CHtmlParser();
	virtual ~CHtmlParser();

public:
	typedef	vector<string>	LocalImg;
	static int LocalImg2Cid(string& body, LocalImg& Imgs);
};

#endif // !defined(AFX_HTMLPARSER_H__E5AE80A6_480D_4F29_A242_43AE16C4D11E__INCLUDED_)
