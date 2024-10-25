/-  s=signals
/+  signalsio
|%
++  slip
  |=  [vax=vase gen=hoon]
  ^-  vase
  =+  gun=(~(mint ut p.vax) %noun gen)
  [p.gun (need (mack q.vax q.gun))]
::
++  bin
  |%
  ++  stem
    ^-  stem:s
    |=  =bowl:stem:s
    ^-  (quip dart:s vase)
    ?>  ?=([%bin *] here.bowl)
    =/  file=vase  (~(got by deps.bowl) (welp /lib t.here.bowl))
    =+  !<([@t deps=(list (pair term path)) =hoon] file)
    ?>  .=  ~(key by deps.bowl)
        (~(put in (sy (turn deps tail))) (welp /lib t.here.bowl))
    =;  vax=(list vase)
      =.  vax  (snoc vax :(slop !>(signalsio=signalsio) !>(s=s) !>(..zuse)))
      [~ (slip (reel vax slop) hoon)]
    %+  turn  deps
    |=  [fac=term dep=path]
    =/  =vase  (~(got by deps.bowl) dep)
    vase(p [%face fac p.vase])
  --
::
++  lib
  |%
  ++  root
    =,  signalsio
    ^-  root:s
    |=  [=bowl:root:s =stud:s =vase]
    =/  m  (charm:root:s ,pail:s)
    ^-  form:m
    ?+    stud  !!
        [%init ~]
      ~&  >>  %initing
      =+  !<(=@t vase)
      =/  [pax=(list (pair term path)) =hoon]  (build t)
      ;<  ~  bind:m  (replace !>([t pax hoon]))
      ?>  ?=([%lib *] here.bowl)
      =/  dest=path  (welp /bin t.here.bowl)
      =/  sour=(set path)  (sy (turn pax tail))
      =.  sour  (~(put in sour) here.bowl)
      ;<  ~  bind:m  (make-stem dest /bin sour)
      ~&  >  %finished-initing
      done
    ==
  :: TODO: allow optional face and relative paths (i.e. /^/^/path)
  ::
  ++  import-line
    ;~  plug
      (cook term ;~(pfix ;~(plug (jest '/-') gap) sym))
      (cook |=(=path (welp /bin path)) ;~(pfix ;~(plug gap fas) (more fas sym)))
    ==
   ::
   ++  build
     |=  text=@t
     ^-  [(list (pair term path)) hoon]
     (rash text ;~(pfix (star gap) ;~(plug (more gap import-line) vest)))
  --
::
++  counter
  %-  crip
  """
  /-  t  /add/two
  =,  signalsio
  |=  [=bowl:root:s =stud:s =vase]
  =/  m  (charm:root:s ,pail:s)
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
  |=  =bowl:stem:s
  :-  ~
  =+  !<(=@ud (~(got by deps.bowl) /counter))
  !>((mod ud 2))
  """
::
++  parity
  %-  crip
  """
  |=  =bowl:stem:s
  :-  ~
  !>
  ?:  =(0 !<(@ud (~(got by deps.bowl) /is-even)))
    'true'
  'false'
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
