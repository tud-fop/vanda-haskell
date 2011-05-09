(Stand 2011-05-05)



Zur Code-Übersetzung wird das Glorious Glasgow Haskell Compilation System
(GHC) benötigt. Möchte man Profiling betreiben, werden zusätzlich alle
genutzten Bibliotheken in einer Profiling unterstützenden Version gebraucht.



Ubuntu-Installation auf dem Server linux.tcs des Lehrstuhls/Instituts:

$ cat /etc/lsb-release
DISTRIB_ID=Ubuntu
DISTRIB_RELEASE=10.04
DISTRIB_CODENAME=lucid
DISTRIB_DESCRIPTION="Ubuntu 10.04.2 LTS"

$ uname -a
Linux linux.tcs 2.6.32-31-generic #61-Ubuntu SMP Fri Apr 8 18:25:51 UTC 2011
x86_64 GNU/Linux


Es sollte unter Ubuntu ausreichen, folgende Pakete zu installieren:

$ aptitude show ghc6-prof
Package: ghc6-prof
State: installed
Automatically installed: no
Version: 6.12.1-12
Priority: optional
Section: universe/devel
Maintainer: Ubuntu Developers <ubuntu-devel-discuss@lists.ubuntu.com>
Uncompressed Size: 262M
Depends: ghc6 (= 6.12.1-12)

$ aptitude show cabal-install
Package: cabal-install
State: installed
Automatically installed: no
Version: 0.8.0-1
Priority: optional
Section: universe/haskell
Maintainer: Ubuntu Developers <ubuntu-devel-discuss@lists.ubuntu.com>
Uncompressed Size: 7.078k
Depends: libc6 (>= 2.4), libffi5 (>= 3.0.4), libgmp3c2, zlib1g (>= 1:1.1.4)


Wichtige Haskell-Pakete:

deepseq-1.1.0.2
heap-1.0.0
parsec-3.1.1

Installierbar mit cabal (Option -p zur zusätzlichen Erstellung der
Profiling-Bibliotheken):

$ cabal install -p deepseq heap parsec



Zur Erstellung der Dokumentation wird Haddock verwendet:

haddock-2.8.1

Auch Haddock kann per cabal installiert werden:

$ cabal install -p haddock



Bei Übersetzungsfehlern mit GHC von der Form

    Could not find module `<module>':
      It is a member of the hidden package `<package>-<version>'.
      Use -v to see a list of the files searched for.

einfach das Kommandozeilenargument "-package <package>" ergänzen.
