;==================
Macaddr For Erlang
;==================
See docs/index.html for api and more detailed information.


What is this?
---------------------------

A cross platform MAC Address retriever for Erlang.
It is inspired by the macaddr gem for ruby (see http://rubyforge.org/projects/codeforpeople)


How do I use it?
---------------------------

To get a single MAC address

  1> macaddr:address().
     "00:32:7c:02:34:61"

  2> macaddr:address_list().
     ["00:32:7c:02:34:61", "00:2d:05:04:55:63"]



How do I build it?
---------------------------
make
make test   # will also make clean before running tests
make docs



How do I install it?
---------------------------
After building it, there are some options on using it.

1. You can modify your code path by adding the following like to ~/.erlang
    code:add_pathz("/wherever/you/built/macaddr").

2. Use the macaddr directory as-is in your erlang/lib.
     sudo mv macaddr /usr/local/lib/erlang/lib

3. Clone from git in your erlang/lib directory and keep macaddr up to date using git
     cd /usr/local/lib/erlang/lib
     sudo git clone  <macaddr git url> macaddr
     cd macaddr
     sudo make
