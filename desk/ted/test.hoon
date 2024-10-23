/-  spider
/+  *strandio, signals
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
;<  our=@p  bind:m  get-our
;<  ~  bind:m
  %+  poke  [our %signals] 
  make-root+!>([/fil/add/two /fil !>(add-two:signals)])
;<  ~  bind:m
  %+  poke  [our %signals] 
  make-stem+!>([/bin/add/two /bin (sy ~[/fil/add/two])])
;<  ~  bind:m
  %+  poke  [our %signals] 
  make-root+!>([/fil/nod/counter /fil !>(counter:signals)])
;<  ~  bind:m
  %+  poke  [our %signals] 
  make-stem+!>([/bin/nod/counter /bin (sy ~[/bin/add/two /fil/nod/counter])])
;<  ~  bind:m
  %+  poke  [our %signals] 
  make-root+!>([/fil/nod/is-even /fil !>(is-even:signals)])
;<  ~  bind:m
  %+  poke  [our %signals] 
  make-stem+!>([/bin/nod/is-even /bin (sy ~[/fil/nod/is-even])])
;<  ~  bind:m
  %+  poke  [our %signals] 
  make-root+!>([/fil/nod/parity /fil !>(parity:signals)])
;<  ~  bind:m
  %+  poke  [our %signals] 
  make-stem+!>([/bin/nod/parity /bin (sy ~[/fil/nod/parity])])
;<  ~  bind:m
  %+  poke  [our %signals] 
  make-root+!>([/counter /number !>(10)])
;<  ~  bind:m
  %+  poke  [our %signals] 
  make-stem+!>([/is-even /number (sy ~[/counter])])
;<  ~  bind:m
  %+  poke  [our %signals] 
  make-stem+!>([/parity /txt (sy ~[/is-even])])
(pure:m !>(~))
