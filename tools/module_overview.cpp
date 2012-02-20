#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <iostream>
#include <string>
#include <sys/stat.h>
#include <fstream>
#include <vector>
#include <stdlib.h>
#include <map>


using namespace std;

int build_graph(vector<string> &modules,string project_name)
{
typedef std::map <std::string, vector<string> > MapType;
typedef std::vector <std::string>  VectorType;
MapType moduleImports;
//MapType hImports = moduleImports;
  for (int i=0; i<modules.size(); i++)				// berechne eine Map mit den Listen (Vektoren) von Imports pro Modul
  {	
	string module = modules.at(i);
	int pos_dot_end = module.rfind(".");
	string module_name = module.substr(0,pos_dot_end);
  	int pos_dot_file = module_name.substr(0,pos_dot_end).rfind(project_name);
	module_name = module_name.substr(pos_dot_file+project_name.size()+1,module_name.size());
	int pos_dot = module_name.find("/"); 
		while (pos_dot != -1)
		{
			module_name = module_name.substr(0,pos_dot) + "." + module_name.substr(pos_dot+1,module_name.size());
			pos_dot = module_name.find("/");
		}
	//cout << "Modulname: " << module_name << " pos_dot_end: " << pos_dot_end << " pos_dot_file: "<< pos_dot_file <<  " substring: "<< module.substr(0,pos_dot_end) <<"\n";
	//cout << "---" << module_name <<" importiert: " << endl;
	
	//vector<string> vec (0);
	//hImports.insert( pair<string,vector<string> >(module_name,vec) );
	//cout << hImports.size()<< endl;

	fstream myfile;
	fstream graph_file;
  	string line;
  	myfile.open (modules.at(i).c_str());
  	graph_file.open("modules.dot",ios::out | ios::app);
	if (!myfile.is_open())
  	{
  	  cout << "something went wrong while reading the file " << modules.at(i) <<"\n";
  	}
  	else
  	{	
		graph_file << "\"" << module_name   <<"\";\n";
		vector<string> importlist;
		while (myfile.good())
		{	
			getline (myfile, line);
			int pos=  line.find("import");
			//cout << line <<endl;
			if (pos != -1)
			{
				if (line.substr(0,pos).find("--")== -1)
				{
					string import = line.substr(pos+7,line.size());
					int pos_qual = import.find("qualified");
					if (pos_qual != -1)
					{
						import = import.substr(10,import.size());
					}
					int pos_del = import.find_first_not_of(" ");
					if (pos_del != -1) 
					import = import.substr(pos_del,import.size());
					pos_del = import.find(" ");
					if (pos_del != -1)
					{
						import = import.substr(0,pos_del);
					}
					string test_import = import;
					int pos_dot = import.find("."); 
					while (pos_dot != -1)
					{
						test_import = test_import.substr(0,pos_dot) + "/" + test_import.substr(pos_dot+1,test_import.size());
						pos_dot = test_import.find(".");
					}
					//cout << import <<"\n";
					//cout << test_import << "\n";
					
					for(int j = 0; j<modules.size(); j++)
					{
						string importing_module = modules.at(j);
						int pos_proName = importing_module.find(project_name);
						if (pos_proName != -1)
						{
							importing_module = importing_module.substr(pos_proName+ project_name.size()+1,importing_module.size());
							//cout << "Das importierende Modul: " << importing_module <<endl; 
						}
						pos_dot = importing_module.rfind(".");
						if (pos_dot !=-1)
						{
							importing_module = importing_module.substr(0,pos_dot);
						} 
						//cout << "-------------" << importing_module << "----------"<< endl;
						//cout << "+++++++++++++" <<test_import << endl;
						if (importing_module.compare(test_import) == 0)
						{
							//graph_file << "\"" << module_name + "\" -> \"" + import + "\";\n";
							moduleImports[module_name].push_back(import);
							importlist.push_back(import);
							//cout << import << endl ;
						}
					}
				}
			}
		}
		/*if(importlist.size()>1)
		{
			graph_file << "{ rank = same; ";
			for (int j = 0; j< importlist.size(); j++)
			{
				graph_file << "\"" << importlist.at(j) << "\"; ";
			}
			graph_file << "}\n";
	  	}*/
	}
	//cout << endl;
	graph_file.close();
  }
  MapType mItC = moduleImports; //Transitive Hülle aller Imports
  MapType hImps = moduleImports;
  //MapType::iterator iter = mItC.begin();
  MapType::const_iterator end = moduleImports.end();
  MapType justTrans;
  MapType hImports = moduleImports;
  //cout << "originale Importliste:"  << endl;
  for (MapType::const_iterator it = moduleImports.begin(); it != end; it++) {
	for(int k = 0; k < it->second.size(); k++){
	//	cout << it->first << " -> " << it->second.at(k) << endl;;
	}
  }

//  cout << "size: "<< hImports.size() << " hImports: " <<endl;
//  end = hImports.end();
//  for (MapType::const_iterator it = hImports.begin(); it != end; it++) {
//	for(int k = 0; k < it->second.size(); k++){
//		cout << it->first << " -> " << it->second.at(k) << endl;
//	}
//  }
//  cout << endl <<endl;
 
  //cout << "-----------1----------" << endl;
  end = mItC.end();
  bool no_changes = false;
  while (no_changes == false){
  	no_changes = true;
	for (MapType::const_iterator it = mItC.begin(); it != end; it++) {
		//cout << "Iterator:"  << " end: " << end << endl;
		//cout<< "ebene1, "<<  "Iterator:" << it->first <<endl <<endl;
		for(int i = 0; i < (it->second.size()); i++){
			//cout << "ebene2"<< " Länge der Liste: "<< it->second.size() << endl;
			
			string transIter = it->second.at(i);
			if (mItC.count(transIter) != 0){
			vector<string> transVal = mItC.find(transIter)->second;
			//cout << "test" << endl;
				for (int z = 0; z < transVal.size(); z++){
					//cout<< "ebene3"<<endl;
					//cout << "+++++  " << i << ","  << z <<"  ++++++++"<< endl;
					//cout << it->first << "," << transVal.at(z)<<endl;
					bool already_contained = false;
					if (justTrans.count(it->first) != 0){
						for (int j = 0; j< justTrans[it->first].size(); j++){
							//cout<< "ebene4"<<endl;
							//cout << "+++++++ " << mItC[it->first].at(j) << "," << transVal.at(z) << " ++++++" << endl;
							//if (it->first == "Demo") cout << "Demo: "<< transVal.at(z) <<endl;
							if (justTrans[it->first].at(j) == transVal.at(z)){
								already_contained = true;
//								cout << "+++++++ " << it->first << "++"<< justTrans[it->first].at(j) << "," << transVal.at(z) << " ++++++" << endl;
								break;
							}
						}
					}
					if (already_contained == false){
						hImports[it->first].push_back(transVal.at(z));
						justTrans[it->first].push_back(transVal.at(z));
						no_changes = false;
						//cout << ">>>>>>>>>>>>>neuer Eintrag <<<<<<<<<<<" << endl ; 
					}
				}
			}
			//cout << it->second.at(i) << endl;
		}
//		cout << endl;
  	}
	mItC = hImports;
  }
  //cout << "-----------2----------" << endl;
  cout <<endl<<endl<< "Transitive Hülle ohne Original:"  << endl;
  MapType::const_iterator end3 = justTrans.end();
  for (MapType::const_iterator it = justTrans.begin(); it != end3; it++) {
	for(int k = 0; k < it->second.size(); k++){
	//	cout << it->first << " -> " << it->second.at(k) << endl;;
	}
  }
  //cout << "-----------3----------" << endl;
  
  end = justTrans.end();
  for (MapType::const_iterator it = justTrans.begin(); it != end; it++){
//	cout <<"test1"<<endl;
	//int count_ch = 0;
	for(int i = 0; i < (it->second.size()); i++){
//		cout <<"test2"<<endl;
		//if (justTrans.count(it->first) != 0){
			for (int j = 0; j< moduleImports[it->first].size(); j++){
//				cout <<"test3"<<endl;
				if (it->second.at(i) == moduleImports[it->first].at(j)){
					//cout <<"test4"<<endl;
					//vector<string> myvector = moduleImports[it->first];
					//cout <<" vector allocated"<< endl;
					//cout << "<<<<<" << it->first << " , " << moduleImports[it->first].at(j) <<endl;
					moduleImports[it->first].erase(moduleImports[it->first].begin() + j);
					//cout << "element erased" <<endl;
					//count_ch++;
					break;
				}
			} 
		//}
	}
  }

 // cout << "-----------4----------" << endl;

  end3 = moduleImports.end();
  cout << endl <<endl << "final Imports: " << endl;
  for (MapType::const_iterator it = moduleImports.begin(); it != end3; it++) {
	for(int k = 0; k < it->second.size(); k++){
	//	cout << it->first << " -> " << it->second.at(k) << endl;;
	}
  }

  // Graphen malen:

  fstream graph_file;
  graph_file.open("modules.dot",ios::out | ios::app);

  end = moduleImports.end();
  for (MapType::const_iterator it = moduleImports.begin(); it != end; it++) {
	for(int k = 0; k < it->second.size(); k++){
		graph_file << "\"" << it->first + "\" -> \"" + it->second.at(k) + "\";\n";
		//cout << it->first << " -> " << it->second.at(k) << endl;;
	}
	if(it->second.size()>1)
	{
		graph_file << "{ rank = same; ";
		for (int j = 0; j< it->second.size(); j++)
		{
			graph_file << "\"" << it->second.at(j) << "\"; ";
		}
		graph_file << "}\n";
  	}	
  }

}

int file_handle(string path,string filename,vector<string> &modules)
{
 //cout << filename << "\n";
 int pos = filename.find(".");
 if(pos!= -1){
 int length = filename.size() - pos;
 string filetype = filename.substr( pos+1, length) ;
 //cout <<"\n--------"<< filetype << "-------\n";
 if (filetype == "lhs" || filetype == "hs")
 {
  modules.push_back(path);
 }

 }
 return 0;
}

int rec_path(string path,vector<string> &modules)
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
          file_handle(newpath,filename,modules); 
        }
        if (S_ISDIR(Status.st_mode) && filename != "." && filename != ".." && filename !=".git")  {
			//cout << filename<<endl;
                  rec_path(newpath,modules); 
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
  vector<string> modules;
  fstream graph;
  if (argc<2){
  cout << "Bitte geben sie den Namen des Projektordners an: \n";
  cin >> project_name;
  }
  else project_name = argv[1];
//  project_name = "praxis1-source";
  string path = "./" + project_name;
  
  rec_path(path,modules);
  
  graph.open("modules.dot", ios::out|ios::trunc);
  graph << "digraph G { \n ranksep = 4.0;\n";
  graph.close();
  /*cout << "modules contains:";
  for (int i=0; i<modules.size(); i++)
    cout << " " << modules.at(i)<< endl;

  cout << endl << modules.size() << endl << endl;
  */

  build_graph(modules,project_name);
  graph.open("modules.dot", ios::out | ios::app );
  graph << "\n\n}";
  graph.close();


}


