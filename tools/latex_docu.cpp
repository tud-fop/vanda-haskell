#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <iostream>
#include <string>
#include <sys/stat.h>
#include <stdlib.h>

using namespace std;

int file_handle(string path,string filename)
{
// cout << filename << "\n";
 int pos = filename.find(".");
 if(pos!= -1){
// cout << filename << "\n";
// cout << "\n------" << pos << "-------\n";
 int length = filename.size() - pos;
// cout << length;
 string filetype = filename.substr( pos+1, length) ;
// cout <<"\n--------"<< filetype << "-------\n";
 if (filetype == "lhs" || filetype == "tex")
 {
   string command = "pdflatex -output-directory ./LaTeX_Documentation " + path;
   system(command.c_str());
 }

// cout <<"Dies ist eine Datei";
// system("pdflatex ");
 }
 return 0;
}

int rec_path(string path)
{
  DIR *dp;
  struct dirent *ep;     
  dp = opendir (path.c_str());
  
  if (dp != NULL)
  {
    while (ep = readdir (dp))
      {
      string  filename = ep->d_name;
      struct stat Status;
      string newpath;
      if (filename != "." && filename != ".."){
	newpath = path + "/" + filename;
        stat(newpath.c_str(), &Status);
        int Dateityp = Status.st_mode & S_IFMT;
        if (S_ISREG(Status.st_mode))
        { 
          file_handle(newpath,filename); 
        }
        if (S_ISDIR(Status.st_mode) && filename != "." && filename != ".." && filename!=".git")  {
                  rec_path(newpath); 
        }
      }
      }
    closedir (dp);
  }
  else{
    perror ("Couldn't open the directory");
    return -1;
  }
  return 0;
}

int main (int argc, char *argv[])
{ 
  string project_name = "";
  if (argc<2){
  project_name = "./";
  }
  else project_name = argv[1];
//  project_name = "praxis1-source";
  string path = project_name ;
  
  return rec_path(path);
}


