|%
++  usergroup
  %-  crip
  """
  :-  /group
  =,  grubberyio
  ^-  base:g
  |=  [=bowl:base:g =stud:g =vase]
  =/  m  (charm:base:g ,pail:g)
  ^-  form:m
  ?>  ?=([%sig ~] stud)
  (pour !>(!<((set @p) vase)))
  """
::
++  group-perm
  %-  crip
  """
  :-  /perm
  =,  grubberyio
  ^-  base:g
  |=  [=bowl:base:g =stud:g =vase]
  =/  m  (charm:base:g ,pail:g)
  ^-  form:m
  ?>  ?=([%sig ~] stud)
  (pour !>(!<(perm vase)))
  """
::
++  counter-container
  %-  crip
  """
  :-  /sig
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
      (overwrite-base counter /counter `!>(10))
    ;<  ~  bind:m
      (overwrite-stem is-even /is-even (sy ~[counter]))
    ;<  ~  bind:m
      (overwrite-stem parity /parity (sy ~[is-even]))
    done
  ==
  """
::
++  counter
  %-  crip
  """
  /-  t  /add/two
  :-  /ud
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
  :-  /loob
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
  :-  /txt
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
