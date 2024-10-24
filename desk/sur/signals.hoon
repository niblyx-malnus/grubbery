|%
+$  stud  path
+$  pail  (pair stud vase)
+$  card  card:agent:gall
:: +$  make
::   $%  [%root =path =pail]
::       [%stem =path sour=(set path)]
::   ==
:: effects that a root can emit
::
+$  dart
  $%  [%poke =wire =path =pail]
      [%bump =wire =path =pail]
      [%peek =wire =path]
      :: [%make =wire =make]
      :: [%kill =wire =path]
      :: [%tidy =wire =path]
      [%sysc =card:agent:gall]
  ==
::
+$  bolt  (pair path dart)
::
:: perms $-(ship ?) crash is no, bubbles down
::                  catches incoming reads and pokes
::                  goes down, keeps checking until a yes
:: sandbox $-((unit path) ?) crash is no, bubbles up
::                  catches outgoing effects
::                  goes up, keeps checking until a no
::
+$  kind
  $%  [%root proc=(unit proc:root)]
      [%stem tidy=[flag=? boom=(unit tang)] sour=(map path @da)]
  ==
::
+$  node  [data=vase =stud =kind]
+$  land  (axal node)
+$  give  [from=path =wire]
+$  poke  [=give =pail]
+$  tack
  $:  last=@da
      sinx=(set path)
      give=(unit give) :: add some identifier here
      line=(qeu poke)
  ==
+$  trac  (axal tack)
::
++  root
  =<  root
  |%
  +$  bowl
    $:  now=@da   :: time
        our=@p    :: host
        eny=@uvJ  :: entropy
        :: wex= TODO
        :: sup= TODO
        from=path :: provenance TODO: make sure this is accurate
        here=path :: our address
    ==
  ::
  +$  sign
    $%  [%poke p=(each pail goof)]
        [%bump p=(unit tang)]
    ==
  ::
  +$  intake
    $%  [%bump =pail]
        [%peek =wire =path =land] :: local read
        [%root =wire =sign]
        :: responses to / expectation of syscalls
        :: we should make sure these get back to
        :: the process that started them and not
        :: some subsequent one
        ::
        [%arvo =wire sign=sign-arvo]
        [%agent =wire =sign:agent:gall]
        [%watch =path]
        [%leave =path]
    ==
  ::
  +$  input  [=bowl state=vase in=(unit intake)]
  ::
  ++  output-raw
    |*  value=mold
    $~  [~ !>(~) %done *value]
    $:  darts=(list dart)
        state=vase
        $=  next
        $%  [%wait ~]
            [%skip ~]
            [%cont self=(form-raw value)]
            [%fail err=(pair term tang)]
            [%done =value]
        ==
    ==
  ::
  ++  form-raw
    |*  value=mold
    $-(input (output-raw value))
  ::
  +$  proc  _*form:(charm ,pail)
  +$  root  $-([bowl pail] proc)
  +$  give  [from=path =wire]
  +$  poke  [=give =pail]
  :: 
  +$  root-data  [proc=(unit (pair proc give)) line=(qeu poke)]
  ::
  ++  charm
    |*  value=mold
    |%
    ++  output  (output-raw value)
    ++  form    (form-raw value)
    :: give value; leave state unchanged
    ::
    ++  pure
      |=  =value
      ^-  form
      |=  input
      ^-  output
      [~ state %done value]
    ::
    ++  bind
      |*  b=mold
      |=  [m-b=(form-raw b) fun=$-(b form)]
      ^-  form
      |=  =input
      =/  b-res=(output-raw b)  (m-b input)
      ^-  output
      :-  darts.b-res
      :-  state.b-res
      ?-    -.next.b-res
        %wait  [%wait ~]
        %skip  [%skip ~]
        %cont  [%cont ..$(m-b self.next.b-res)]
        %fail  [%fail err.next.b-res]
        %done  [%cont (fun value.next.b-res)]
      ==
    ::
    ++  eval
      |%
      +$  result
        $%  [%next ~]
            [%fail err=(pair term tang)]
            [%done =value]
        ==
      ::
      ++  take
        =|  darts=(list dart)
        |=  [=form =input]
        ^-  [[(list dart) vase result] _form]
        =/  =output  (form input)
        =.  darts  (weld darts darts.output)
        ?:  ?=(%cont -.next.output)
          %=  $
            form   self.next.output
            input  [bowl.input state.output ~]
          ==
        :_  form
        :-  darts
        :-  state.output
        ?-  -.next.output
            %wait  [%next ~]
            %skip  [%next ~]
            %fail  [%fail err.next.output]
            %done  [%done value.next.output]
        ==
      --
    --
  ::
  ++  ingest
    |=  [=proc =bowl state=vase in=(unit intake)]
    =/  m  (charm ,pail)
    ^-  [[(list dart) vase result:eval:m] ^proc]
    =/  res  (mule |.((take:eval:m proc bowl state in)))
    ?-  -.res
      %&  p.res
      %|  [[~ state [%fail %crash p.res]] proc]
    ==
  --
::
++  stem
  =<  stem
  |%
  +$  stem  $-(bowl (quip dart vase))
  +$  bowl
    $:  now=@da              :: time
        our=@p               :: host
        eny=@uvJ             :: entropy
        here=path            :: our address
        deps=(map path vase) :: dependencies
    ==
  --
--
