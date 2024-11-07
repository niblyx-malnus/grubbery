|%
++  counter-container
  %-  crip
  """
  =,  grubberyio
  |=  [=bowl:base:g =stud:g =vase]
  =/  m  (charm:base:g ,pail:g)
  ^-  form:m
  ?+    stud  !!
      [%sig ~]
    =/  counter=path  (weld here.bowl /counter)
    =/  is-even=path  (weld here.bowl /is-even)
    =/  parity=path   (weld here.bowl /parity)
    ;<  ~  bind:m
      (overwrite-base counter /ud /counter `!>(10))
    ;<  ~  bind:m
      (overwrite-stem is-even /loob /is-even (sy ~[counter]))
    ;<  ~  bind:m
      (overwrite-stem parity /txt /parity (sy ~[is-even]))
    done
  ==
  """
::
++  counter
  %-  crip
  """
  /-  t  /add/two
  =,  grubberyio
  |=  [=bowl:base:g =stud:g =vase]
  =/  m  (charm:base:g ,pail:g)
  ^-  form:m
  ?+    stud  !!
      [%inc ~]
    ;<  a=@ud  bind:m  (get-state-as @ud)
    (pour !>(+(a)))
    ::
      [%two ~]
    ;<  a=@ud  bind:m  (get-state-as @ud)
    (pour !>((two:t a)))
  ==
  """
::
++  is-even
  %-  crip
  """
  =,  grubberyio
  |=  =bowl:stem:g
  :-  ~
  =/  deps  ~(tap in ~(key by deps.bowl))
  ?>  ?=(^ deps)
  =+  !<(=@ud (nead (~(got by deps.bowl) i.deps)))
  !>(=(0 (mod ud 2)))
  """
::
++  parity
  %-  crip
  """
  =,  grubberyio
  |=  =bowl:stem:g
  :-  ~
  =/  deps  ~(tap in ~(key by deps.bowl))
  ?>  ?=(^ deps)
  ?:  !<(? (nead (~(got by deps.bowl) i.deps)))
    !>('true')
  !>('false')
  """
::
++  add-two
  %-  crip
  """
  |%
  ++  two  |=(a=@ud (add 2 a))
  --
  """
--
