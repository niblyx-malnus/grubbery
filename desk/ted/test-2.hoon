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
  make-root-2+!>([/fil/add/two /fil !>(add-two:signals)])
;<  ~  bind:m
  %+  poke  [our %signals] 
  make-root-2+!>([/fil/nod/counter /fil !>(counter:signals)])
;<  ~  bind:m
  %+  poke  [our %signals] 
  make-root-2+!>([/fil/nod/is-even /fil !>(is-even:signals)])
;<  ~  bind:m
  %+  poke  [our %signals] 
  make-root-2+!>([/fil/nod/parity /fil !>(parity:signals)])
;<  ~  bind:m
  %+  poke  [our %signals] 
  make-root-2+!>([/counter /number !>(10)])
;<  ~  bind:m
  %+  poke  [our %signals] 
  make-stem-2+!>([/is-even /number (sy ~[/counter])])
;<  ~  bind:m
  %+  poke  [our %signals] 
  make-stem-2+!>([/parity /txt (sy ~[/is-even])])
(pure:m !>(~))
