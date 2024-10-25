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
  %+  poke  [our %signals]
  make-root+!>([/lib/add/two /lib !>(~)])
;<  *  bind:m
  %+  poke-response  /
  poke-root+!>([/ /lib/add/two /init !>(add-two:signals)])
;<  *  bind:m
  %+  poke  [our %signals]
  make-root+!>([/lib/nod/counter /lib !>(~)])
;<  *  bind:m
  %+  poke-response  /
  poke-root+!>([/ /lib/nod/counter /init !>(counter:signals)])
;<  *  bind:m
  %+  poke  [our %signals]
  make-root+!>([/lib/nod/is-even /lib !>(~)])
;<  *  bind:m
  %+  poke-response  /
  poke-root+!>([/ /lib/nod/is-even /init !>(is-even:signals)])
;<  *  bind:m
  %+  poke  [our %signals]
  make-root+!>([/lib/nod/parity /lib !>(~)])
;<  *  bind:m
  %+  poke-response  /
  poke-root+!>([/ /lib/nod/parity /init !>(parity:signals)])
;<  *  bind:m
  %+  poke  [our %signals]
  make-root+!>([/counter /number !>(10)])
;<  *  bind:m
  %+  poke  [our %signals]
  make-stem+!>([/is-even /number (sy ~[/counter])])
;<  *  bind:m
  %+  poke  [our %signals]
  make-stem+!>([/parity /txt (sy ~[/is-even])])
;<  =bowl:strand  bind:m  get-bowl
(pure:m !>(~))
::
|%
++  poke-response
  |=  [=wire req=cage]
  =/  m  (strand ,pail:s)
  ^-  form:m
  ~&  >>  %pokeing
  ;<  our=@p    bind:m  get-our
  =/  =path     (welp /poke/(scot %p our) wire)
  =/  =dock     [our %signals]
  ~&  >>  %watching
  ;<  ~         bind:m  (watch /poke dock path)
  ~&  >>  %poking
  ;<  ~         bind:m  (poke dock req)
  ~&  >>  %awaiting
  ;<  rep=cage  bind:m  (take-fact /poke)
  ;<  ~         bind:m  (take-kick /poke)
  ~&  >>  %received-poke
  ?>  ?=(%sign-root p.rep)
  =+  !<(=sign:root:s q.rep)
  ?>  ?=(%poke -.sign)
  ?-  -.p.sign
    %|  (strand-fail p.p.sign)
    %&  (pure:m p.p.sign)
  ==
--
