# Spartakiade 2018 - Haskell Workshop
<p align="center"><img src="spartakiade.png" width=100/></p>

**18. und 18. März in Berlin**

Web:     http://spartakiade.org/

Twitter: http://twitter.com/spartakiade_org

---

[Workshop Material](./Workshop.md)

---

## Linksammlung

### Tools
- [Stack](https://docs.haskellstack.org/en/stable/README/)
- [Intero](https://haskell-lang.org/intero)
- [VS.Code & Haskelly](https://marketplace.visualstudio.com/items?itemName=Vans.haskero)


### Hilfreiches
- S. Diehl: [Numeric Tower](http://dev.stephendiehl.com/hask/#numeric-tower)
- J. Bailey: [Haskell CheatSheet](http://blog.codeslower.com/static/CheatSheet.pdf)

### Hintergrund
- S. Diehl: [What I wish I knew when learning Haskell](http://dev.stephendiehl.com/hask/)
- Wikipedia: [Non-strict evaluation](https://en.wikipedia.org/wiki/Evaluation_strategy#Non-strict_evaluation)
- Hudak, Hughes, Jones, Wadler: [A History of Haskell: Being Lazy With Class](http://haskell.cs.yale.edu/wp-content/uploads/2011/02/history.pdf)
- Haskell-Wiki: [Lazy vs. non-strict](https://wiki.haskell.org/Lazy_vs._non-strict)
- H. Apfelmus:  [Incomplete Guide to Lazy Evaluation](https://hackhands.com/guide-lazy-evaluation-haskell/)
- [Overloading in Haskell](http://www.cse.chalmers.se/edu/year/2016/course/TDA452_Functional_Programming/lectures/OverloadingAndTypeClasses.html)

## Installation

### Stack
Ich empfehle Haskell nur über das [**Stack Tool**](https://docs.haskellstack.org/en/stable/README/) zu installieren
(also nicht die Plattform oder den GHC direkt).

In jeden Fall solltet ihr nach der Installation mit

```
stack --version
stack update
stack upgrade
```

prüfen (die Version jetzt gerade ist 1.4.0) und eventuell *Upgraden*
(falls nötig).

#### Windows
Siehe [Install and Upgrade Windows](https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows)

Es gibt Installer für

- [64-bit](https://www.stackage.org/stack/windows-x86_64)
- [32-bit](https://www.stackage.org/stack/windows-i386)

Das Installationsverzeichnis solltet Ihr in eure `PATH` Umgebungsvariable 
aufnehmen.

#### MacOS
Zum Mac kann ich leider nicht viel sagen, aber eine
Anleitung findet ihr [hier](https://docs.haskellstack.org/en/stable/install_and_upgrade/#macos)

#### Linux
Für **Ubuntu** und co. gibt es eine `deb` Quelle: `deb http://download.fpcomplete.com/ubuntu xenial main`

Ansonsten bitte unter [How to install](https://docs.haskellstack.org/en/stable/README/#how-to-install)
schauen.

### Intero
Intero ist meiner Meinung nach aktuelle die beste Möglichkeit guten
Editor-Support für Haskell zu bekommen.

Eine Installationsleitung findet ihr [hier](https://haskell-lang.org/intero).

Übrigens: dort steht viel über Emacs - das unten vorgestellte *VS.code* Plugin
nutzt aber auch Intero.