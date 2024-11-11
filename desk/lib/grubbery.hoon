/-  g=grubbery
/+  grubberyio, gui, *examples
|%
++  slip
  |=  [vax=vase gen=hoon]
  ^-  vase
  =+  gun=(~(mint ut p.vax) %noun gen)
  [p.gun (need (mack q.vax q.gun))]
:: from rudder (paldev)
::
++  decap  ::  strip leading base from full site path
  |=  [base=(list @t) site=(list @t)]
  ^-  (unit (list @t))
  ?~  base  `site
  ?~  site  ~
  ?.  =(i.base i.site)  ~
  $(base t.base, site t.site)
::
++  clean-perm
  |=  [here=path =perm:g]
  ^+  perm
  |^
  :*  (clean make.perm)
      (clean poke.perm)
      (clean peek.perm)
  ==
  :: all subdirectories of here are converted to here
  ::
  ++  clean
    |=  pax=(set path)
    ^-  (set path)
    %-  ~(gas in *(set path))
    :-  here
    %+  murn  ~(tap in pax)
    |=  =path
    ?^  (decap here path)
      ~
    [~ path]
  --
::
++  check-pax
  |=  [dest=path pax=(list path)]
  ^-  ?
  ?~  pax
    %|
  ?^  (decap i.pax dest)
    %&
  $(pax t.pax)
::
++  check-pax-hard
  |=  [dest=path pax=(list path)]
  ^-  ?
  ?~  pax
    %|
  ?:  ?=([~ ^] (decap i.pax dest))
    %&
  $(pax t.pax)
::
++  allowed
  |=  [=dart:g perm=(unit perm:g)]
  ^-  ?
  ?~  perm  %&
  ?:  ?=(?(%sysc %scry) -.dart)  %|
  ?-    -.load.dart
      ?(%make %oust)
    (check-pax path.dart ~(tap in make.u.perm))
      ?(%poke %bump %kill)
    (check-pax path.dart ~(tap in poke.u.perm))
      %peek
    (check-pax path.dart ~(tap in peek.u.perm))
  ==
::
++  bin
  |%
  :: bin base does nothing; it's like a rock
  ::
  ++  base
    =,  grubberyio
    ^-  base:g
    |=  [=bowl:base:g =stud:g =vase]
    =/  m  (charm:base:g ,pail:g)
    ^-  form:m
    done
    ::
  ++  stem
    =,  grubberyio
    ^-  stem:g
    |=  =bowl:stem:g
    ^-  (quip dart:g vase)
    ?>  ?=([%bin *] here.bowl)
    =/  grubbery=vase  (nead (~(got by deps.bowl) /bin/grubbery))
    =/  file=vase  (nead (~(got by deps.bowl) [%lib t.here.bowl]))
    =+  !<([@t res=(each [deps=(list (pair term path)) =hoon] tang)] file)
    ?:  ?=(%| -.res)
      ~|("hoon parsing failure" (mean p.res))
    =/  deps=(set path)
      %-  ~(gas in (sy (turn deps.p.res tail)))
      ~[/bin/grubbery [%lib t.here.bowl]]
    ?>  =(deps ~(key by deps.bowl))
    =;  vax=(list vase)
      [~ (slip (reel (snoc vax grubbery) slop) hoon.p.res)]
    %+  turn  deps.p.res
    |=  [fac=term dep=path]
    =/  =vase  (nead (~(got by deps.bowl) dep))
    vase(p [%face fac p.vase])
  --
::
++  lib
  |%
  ++  base
    =,  grubberyio
    ^-  base:g
    |=  [=bowl:base:g =stud:g =vase]
    =/  m  (charm:base:g ,pail:g)
    ^-  form:m
    ?+    stud  !!
        [%sig ~]
      =+  !<(=@t vase)
      =/  res=(each [pax=(list (pair term path)) =hoon] tang)
        (mule |.((build t)))
      ;<  ~  bind:m  (replace !>([t res]))
      ?>  ?=([%lib *] here.bowl)
      =/  dest=path  [%bin t.here.bowl]
      =/  sour=(set path)
        ?:(?=(%| -.res) ~ (sy (turn pax.p.res tail)))
      =.  sour  (~(gas in sour) here.bowl /bin/grubbery ~)
      ;<  ~  bind:m  (overwrite-stem dest /bin /bin sour)
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
++  boot
  =*  grubbery-lib  ..bin :: avoid masking by grubberyio
  =*  zuse-core  ..zuse
  =,  grubberyio
  ^-  base:g
  |=  [=bowl:base:g =stud:g =vase]
  =/  m  (charm:base:g ,pail:g)
  ^-  form:m
  ?+    stud  !!
      [%sig ~]
    ;<  ~  bind:m  (overwrite-base /bin/zuse /bin /bin `!>(zuse-core))
    ;<  ~  bind:m  (overwrite-base /bin/grubbery /bin /bin `!>(grubbery-lib))
    ;<  ~  bind:m  (overwrite-lib /add/two add-two)
    ;<  ~  bind:m  (overwrite-stud-lib /ud '@ud')
    ;<  ~  bind:m  (overwrite-stud-lib /loob '?')
    ;<  ~  bind:m  (overwrite-stud-lib /txt '@t')
    ;<  ~  bind:m  (overwrite-stud-lib /dr '@dr')
    ;<  ~  bind:m  (overwrite-stud-lib /manx 'manx')
    ;<  ~  bind:m  (overwrite-stud-lib /sig ',~')
    ;<  ~  bind:m  (overwrite-stud-lib /init ',~')
    :: counter test
    ::
    ;<  ~  bind:m  (overwrite-lib /add/two add-two)
    ;<  ~  bind:m  (overwrite-base-lib /counter counter)
    ;<  ~  bind:m  (overwrite-base-lib /counter-container counter-container)
    ;<  ~  bind:m  (overwrite-stem-lib /is-even is-even)
    ;<  ~  bind:m  (overwrite-stem-lib /parity parity)
    ;<  *  bind:m
      (overwrite-and-poke /counter-container /sig /counter-container ~ /sig !>(~))
    :: gui setup
    ::
    ;<  ~  bind:m  (overwrite-base-lib /gui 'base:gui')
    ;<  ~  bind:m  (overwrite-stud-lib /gui/init ',~')
    ;<  *  bind:m  (overwrite-and-poke /gui /sig /gui ~ /gui/init !>(~))
    ~&  >  "Grubbery booted!"
    done
  ==
--
