/-  g=grubbery, spider
/+  *strandio, grubbery
=,  strand=strand:spider
^-  thread:spider
=<
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
;<  our=@p  bind:m  get-our
;<  *  bind:m
  %+  poke  [our %grubbery]
  make-base+!>([/lib/base/gui /lib /lib ~])
;<  *  bind:m
  %+  poke-response  /
  poke-base+!>([/ /lib/base/gui /init !>('base:gui')])
;<  *  bind:m
  %+  poke  [our %grubbery]
  make-base+!>([/lib/stud/ud /lib /lib ~])
;<  *  bind:m
  %+  poke-response  /
  poke-base+!>([/ /lib/stud/ud /init !>('@ud')])
;<  *  bind:m
  %+  poke  [our %grubbery]
  make-base+!>([/lib/stud/txt /lib /lib ~])
;<  *  bind:m
  %+  poke-response  /
  poke-base+!>([/ /lib/stud/txt /init !>('@t')])
;<  *  bind:m
  %+  poke  [our %grubbery]
  make-base+!>([/lib/stud/sig /lib /lib ~])
;<  *  bind:m
  %+  poke-response  /
  poke-base+!>([/ /lib/stud/sig /init !>(',~')])
;<  *  bind:m
  %+  poke  [our %grubbery]
  make-base+!>([/lib/stud/dr /lib /lib ~])
;<  *  bind:m
  %+  poke-response  /
  poke-base+!>([/ /lib/stud/dr /init !>('@dr')])
;<  *  bind:m
  %+  poke  [our %grubbery]
  make-base+!>([/lib/stud/manx /lib /lib ~])
;<  *  bind:m
  %+  poke-response  /
  poke-base+!>([/ /lib/stud/dr /init !>('manx')])
;<  *  bind:m
  %+  poke  [our %grubbery]
  make-base+!>([/lib/add/two /lib /lib ~])
;<  *  bind:m
  %+  poke-response  /
  poke-base+!>([/ /lib/add/two /init !>(add-two:grubbery)])
;<  *  bind:m
  %+  poke  [our %grubbery]
  make-base+!>([/lib/nod/counter /lib /lib ~])
;<  *  bind:m
  %+  poke-response  /
  poke-base+!>([/ /lib/nod/counter /init !>(counter:grubbery)])
;<  *  bind:m
  %+  poke  [our %grubbery]
  make-base+!>([/lib/nod/is-even /lib /lib ~])
;<  *  bind:m
  %+  poke-response  /
  poke-base+!>([/ /lib/nod/is-even /init !>(is-even:grubbery)])
;<  *  bind:m
  %+  poke  [our %grubbery]
  make-base+!>([/lib/nod/parity /lib /lib ~])
;<  *  bind:m
  %+  poke-response  /
  poke-base+!>([/ /lib/nod/parity /init !>(parity:grubbery)])
;<  *  bind:m
  %+  poke  [our %grubbery]
  make-base+!>([/counter /ud /counter `!>(10)])
;<  *  bind:m
  %+  poke  [our %grubbery]
  make-stem+!>([/is-even /ud /is-even (sy ~[/counter])])
;<  *  bind:m
  %+  poke  [our %grubbery]
  make-stem+!>([/parity /txt /parity (sy ~[/is-even])])
;<  *  bind:m
  %+  poke  [our %grubbery]
  make-base+!>([/gui /sig /gui ~])
;<  *  bind:m
  %+  poke-response  /
  poke-base+!>([/ /gui /init !>(~)])
(pure:m !>(~))
::
|%
++  poke-response
  |=  [=wire req=cage]
  =/  m  (strand ,pail:g)
  ^-  form:m
  ;<  our=@p    bind:m  get-our
  =/  =path     (welp /poke/(scot %p our) wire)
  =/  =dock     [our %grubbery]
  ;<  ~         bind:m  (watch /poke dock path)
  ;<  ~         bind:m  (poke dock req)
  ;<  rep=cage  bind:m  (take-fact /poke)
  ;<  ~         bind:m  (take-kick /poke)
  ?>  ?=(%sign-base p.rep)
  =+  !<(=sign:base:g q.rep)
  ?>  ?=(%poke -.sign)
  ?-  -.p.sign
    %|  (strand-fail p.p.sign)
    %&  (pure:m p.p.sign)
  ==
--
