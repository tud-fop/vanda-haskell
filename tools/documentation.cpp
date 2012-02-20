#include <iostream>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <iostream>
#include <string>
#include <sys/stat.h>
#include <fstream>
#include <stdlib.h>

using namespace std;

int main(int argc, char *argv[])
{
string name = "unbekannt";
string path = "./";
string main = "";
DIR *dp;
  struct dirent *ep;     
  dp = opendir (".");
  
  if (dp != NULL)
  {
	//cout<< ".-.-.";
    while (ep = readdir (dp))
      {
      string  filename = ep->d_name;
      struct stat Status;
      string newpath;
      int pos = filename.find(".");
      if(pos!= -1){
	int length = filename.size() - pos;
	string filetype = filename.substr( pos+1, length) ;
	if (filetype == "cabal")
	{
		fstream myfile;
		myfile.open (filename.c_str());
		if (myfile.is_open())
		{
			string line;
			while (myfile.good())
			{
				getline (myfile, line);
				string origin_line = line;
				std::transform(line.begin(), line.end(),line.begin(), ::tolower);
				//strlwr(line.c_str());
				pos = line.find("name");
				if (pos != -1)
				{
					name = origin_line;
					pos= name.find(":");
					name = name.substr(pos+1,name.size());
					pos= name.find(" ");
					while (pos != -1)
					{
						name = name.substr(pos+1,name.size());
						//cout << "--" << name << "--\n";
						pos= name.find(" ");
					}
				}
				int pos_path = line.find("hs-source-dirs");
				if (pos_path != -1)
				{
					path = origin_line;
					pos_path= path.find(":");
					path = path.substr(pos_path+1,path.size());
					pos_path= path.find(" ");
					while (pos_path != -1)
					{
						path = path.substr(pos_path+1,path.size());
						//cout << "--" << path << "--\n";
						pos_path= path.find(" ");
					}
					//path = path + "/";
					//cout << path;
				}
				int pos_main = line.find("main-is");
				if (pos_main != -1)
				{
					main = origin_line;
					pos_main= main.find(":");
					main = main.substr(pos_main+1,main.size());
					pos_main= main.find(" ");
					while (pos_main != -1)
					{
						main = main.substr(pos_main+1,main.size());
						//cout << "--" << main << "--\n";
						pos_main= main.find(" ");
					}
				}
			}
		}
	}
      }

      }
    //cout << name << ".....";
    closedir (dp);
  }

//cout << "Erstellen der Haddock-Dokumentation...";
string comm = "cd "+ path +" && haddock -h -o ../Haddock_Documentation " + main;
system(comm.c_str());
//cout << "Erstellen der LaTeX-Dokumentation...";
system("mkdir -p LaTeX_Documentation");
system("g++ latex_docu.cpp -o latex_docu.out");
comm = "./latex_docu.out " + path;
system(comm.c_str());
//cout << ("Erstellen der HTML-Projektübersicht...");
comm = "php hierarchical_structure.php "+ path + " "+ name + " > hierarchical_structure.html";
system (comm.c_str());
//cout << "Erstellen der Modulübersicht...";
//comm = "g++ module_overview.cpp -o module_overview.out";
//cout << comm;
//system(comm.c_str());
//comm = "./module_overview.out " + path;
comm = "runhaskell modules.hs " + path;
system(comm.c_str());

string command = "dot module_overview.dot -Tjpg -o module_overview.jpg";
system(command.c_str());
//cout << path << "--" << name<< "--"<< main <<"\n";

}



