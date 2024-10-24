/-  s=signals, spider
/+  *strandio, signals
=,  strand=strand:spider
^-  thread:spider
=<
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
;<  our=@p  bind:m  get-our
;<  *  bind:m
  %+  signals-vent  /
  make-root+!>([/ /fil/add/two /fil !>(add-two:signals)])
;<  *  bind:m
  %+  signals-vent  /
  make-root+!>([/ /fil/nod/counter /fil !>(counter:signals)])
;<  *  bind:m
  %+  signals-vent  /
  make-root+!>([/ /fil/nod/is-even /fil !>(is-even:signals)])
;<  *  bind:m
  %+  signals-vent  /
  make-root+!>([/ /fil/nod/parity /fil !>(parity:signals)])
;<  *  bind:m
  %+  signals-vent  /
  make-root+!>([/ /counter /number !>(10)])
;<  ~  bind:m
  %+  poke  [our %signals]
  make-stem+!>([/is-even /number (sy ~[/counter])])
;<  ~  bind:m
  %+  poke  [our %signals]
  make-stem+!>([/parity /txt (sy ~[/is-even])])
(pure:m !>(~))
::
|%
++  signals-vent
  |=  [=wire req=cage]
  =/  m  (strand ,pail:s)
  ^-  form:m
  ~&  >>  %venting
  ;<  our=@p    bind:m  get-our
  =/  =path     (welp /vent/(scot %p our) wire)
  =/  =dock     [our %signals]
  ~&  >>  %watching
  ;<  ~         bind:m  (watch /vent dock path)
  ~&  >>  %poking
  ;<  ~         bind:m  (poke dock req)
  ~&  >>  %awaiting
  ;<  rep=cage  bind:m  (take-fact /vent)
  ;<  ~         bind:m  (take-kick /vent)
  ~&  >>  %received-vent
  ?>  ?=(%sign-root p.rep)
  =+  !<(=sign:root:s q.rep)
  ?>  ?=(%poke -.sign)
  ?-  -.p.sign
    %|  (strand-fail p.p.sign)
    %&  (pure:m p.p.sign)
  ==
--
