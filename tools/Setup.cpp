#include <iostream>
#include <stdlib.h>
using namespace std;

int main(int argc, char *argv[])
{
	//cout << "Installation des Paketes...";
	system("cabal install");
	system("g++ documentation.cpp -o documentation.out");
	system("./documentation.out");
}



