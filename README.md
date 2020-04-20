# Emacs config

If you're new to emacs, check out
[this introductory tutorial](http://www.braveclojure.com/basic-emacs/)!


## Thanks
This setup was a copy of [Mark Bastian](https://github.com/markbastian) setup and it has somethings based on
[Azer Koçulu](https://github.com/azer) (Especially for golang)

## Organization

I've tried to separate everything logically and document the purpose
of every line. [`init.el`](./init.el) acts as a kind of table of
contents.  It's a good idea to eventually go through `init.el` and the
files under the `customizations` directory so that you know exactly
what's going on. Additionally, you can add other packages via `git submodule` when it's not able to retrieve them using melpa repos; this packages are located inside `libs` directory.

## Supporting CSS, HTML, JS, etc.

Emacs has decent support for CSS, HTML, JS, and many other file types out of the box, but if you want better support, then have a look at [emacs config's init.el](https://github.com/flyingmachine/emacs.d/blob/master/init.el). It's meant to read as a table of contents. The emacs.d as a whole adds the following:

In general, if you want to add support for a language then you should be able to find good instructions for it through Google. Most of the time, you'll just need to install the "x-lang-mode" package for it.


## References
Mark Bastian's emacs setup: https://github.com/markbastian/.emacs.d

Azer Koçulu emacs setup: https://github.com/azer/emacs
