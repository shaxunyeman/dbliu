#include <iostream>
#include <string>
#include <fstream>

using namespace std;

int EmailHeader(ifstream &ifile,string &header)
{
    //get header
    while (ifile.eof() == false)
    {
        char outbuffer[1024] = {0};
        ifile.getline(outbuffer,1024,'\n');
        //std::cout << outbuffer << std::endl;
        header += outbuffer;
        header += "\n";
        if (strcmp(outbuffer,"\r") == 0)
            break;
    }

    return 0;
}

int ProcessHeader(const string &header)
{
    std::cout << header << std::endl;

    return 0;
}

int ExstractBoundaryFrom(const string &header,string &boundary)
{
    size_t pos = header.find("boundary",0);
    if (pos == string::npos)
        return 1;

    boundary = header.substr(pos);
    pos = boundary.find("=",0);
    boundary = boundary.substr(pos + 1);
    //remove \" symbol
    size_t s1 = boundary.find("\"",0);
    size_t s2 = string::npos;
    if (s1 != string::npos)
        s2 = boundary.find("\"",s1 + 1);

    if (s1 != string::npos && s2 != string::npos)
        boundary = boundary.substr(s1 + 1,s2 - s1 - 1);

    return 0;
}

bool hasBoundary(const string &header)
{
    bool bHas = (header.find("boundary") == string::npos) ? false : true;
    return bHas;
}

/*
note: boundary dosen't include any '\r','\n' and '\r\n'
 */
bool isBoundaryEnd(const string &boundary,const string &contentline)
{
    bool bEnd = false;
    string temp = "--";
    temp += boundary;
    temp += "--"; 
    if (contentline.find(temp) != string::npos)
        bEnd = true;
    return bEnd;
}

/*
note: boundary dosen't include any '\r','\n' and '\r\n'
 */
bool isBoundaryBegin(const string &boundary,const string &contentline)
{
    bool bBegin = false;
    string temp = "--";
    temp += boundary;
    if (contentline.find(temp) != string::npos)
        bBegin = true;
    return bBegin;
}

int parseBoundaryZone(const string &boundary,ifstream &ifile)
{
    if (boundary.empty())
        return 1;

    while(ifile.eof() == false)
    {
        char line[4096] = {0};
        ifile.getline(line,4096,'\n');
        if (isBoundaryBegin(boundary,line))
        {
            string header;
            EmailHeader(ifile,header);

            ProcessHeader(header);

            if (hasBoundary(header))
            {
                string bound;
                if (0 == ExstractBoundaryFrom(header,bound))
                    parseBoundaryZone(bound,ifile);
            }
        }

        if (isBoundaryEnd(boundary,line))
            break;
    }
    return 0;
}

string parseEmailFrom(ifstream &ifile)
{
    //get header
    string emailheader;
    EmailHeader(ifile,emailheader);
    std::cout << emailheader << std::endl;

    if (hasBoundary(emailheader))
    {
        string boundary;
        ExstractBoundaryFrom(emailheader,boundary);
        std::cout << boundary << std::endl;

        std::cout << "begin parse boudary zone ......" << std::endl;

        parseBoundaryZone(boundary,ifile);
    }
    else
    {
        //print body text/plan
    }

    return "";
}

int main(int argc,char *argv[])
{

    ifstream infile("test.eml",std::ifstream::binary);
    parseEmailFrom(infile);
    infile.close();
    return 0;
}
