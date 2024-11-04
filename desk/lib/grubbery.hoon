/-  g=grubbery
/+  grubberyio, server, html-utils
|%
++  mx  mx:html-utils
++  kv  kv:html-utils
++  slip
  |=  [vax=vase gen=hoon]
  ^-  vase
  =+  gun=(~(mint ut p.vax) %noun gen)
  [p.gun (need (mack q.vax q.gun))]
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
  =,  grubberyio
  ^-  base:g
  |=  [=bowl:base:g =stud:g =vase]
  =/  m  (charm:base:g ,pail:g)
  ^-  form:m
  ?+    stud  !!
      [%sig ~]
    ;<  ~  bind:m  (overwrite-base /bin/zuse /bin /bin `!>(..zuse))
    ;<  ~  bind:m  (overwrite-base /bin/grubbery /bin /bin `!>(grubbery-lib))
    ;<  ~  bind:m  (overwrite-lib /add/two add-two)
    ;<  ~  bind:m  (overwrite-stud-lib /ud '@ud')
    ;<  ~  bind:m  (overwrite-stud-lib /loob '?')
    ;<  ~  bind:m  (overwrite-stud-lib /txt '@t')
    ;<  ~  bind:m  (overwrite-stud-lib /dr '@dr')
    ;<  ~  bind:m  (overwrite-stud-lib /manx 'manx')
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
::
++  gui
  |%
  ++  con
    |%
    +$  stud  $-(vase manx)
    +$  cone  $-(cone:g manx)
    --
  ::
  ++  make-id  |=(p=path (trip (rap 3 (join '_' p))))
  ::
  ++  counter
    =,  grubberyio
    |=  =bowl:stem:g
    ^-  (quip dart:g vase)
    :-  ~
    =+  !<(=@ud (nead (~(got by deps.bowl) /counter)))
    !>
    ;div
      =id     (make-id here.bowl)
      =class  "flex flex-row gap-2 justify-center"
      ;div(class "text-4xl font-bold mb-6")
        ; counter: {(scow %ud ud)}
      ==
      ;form
        =method  "POST"
        =action  "/grub?action=inc"
        ;button
          =type   "submit"
          =class  "px-3 py-2 bg-blue-500 text-white font-semibold text-lg rounded-lg hover:bg-blue-700 transition duration-200"
          ; +
        ==
      ==
    ==
  ::
  ++  is-even
    =,  grubberyio
    |=  =bowl:stem:g
    ^-  (quip dart:g vase)
    :-  ~
    =+  !<(e=? (nead (~(got by deps.bowl) /is-even)))
    !>
    ^-  manx
    ;div
      =id     (make-id here.bowl)
      =class  "text-4xl font-bold mb-6"
      ; is-even: {?:(e "%.y" "%.n")}
    ==
  ::
  ++  parity
    =,  grubberyio
    |=  =bowl:stem:g
    ^-  (quip dart:g vase)
    :-  ~
    =+  !<(=@t (nead (~(got by deps.bowl) /parity)))
    !>
    ;div
      =id     (make-id here.bowl)
      =class  "text-4xl font-bold mb-6"
      ; parity: {(trip t)}
    ==
  ::
  ++  refresher
    =,  grubberyio
    ^-  base:g
    =<
    |=  [=bowl:base:g =stud:g =vase]
    =/  m  (charm:base:g ,pail:g)
    ^-  form:m
    ?+    stud  !!
        [%interval ~]
      (pour !>(!<(@dr vase)))
      ::
        [%handle-http-request ~]
      =+  !<([request-line:server req=inbound-request:eyre] vase)
      ?+    method.request.req
        %+  pure:m  /simple-payload
        !>((method-not-allowed method.request.req))
        ::
          %'GET'
        =/  since=@da  (slav %da (need (get-key:kv 'since' args)))
        ;<  now=@da  bind:m  get-time
        ;<  interval=@dr  bind:m  (peek-root-as @dr here.bowl)
        =/  until=@da  (add now interval)
        ;<  ~  bind:m  (send-wait until)
        ;<  ~  bind:m  take-wake-or-fast-refresh
        (send-refresh-list since)
      ==
    ==
    ::
    |%
    ++  take-wake-or-fast-refresh
      =/  m  (charm ,~)
      ^-  form:m
      |=  input
      :+  ~  state
      ?+    in  [%skip ~]
        ~  [%wait ~]
        ::
          [~ %arvo [%wait @ ~] %behn %wake *]
        ?~  error.sign.u.in
          [%done ~]
        [%fail %timer-error u.error.sign.u.in]
        ::
          [~ %bump *]
        ?.  =(p.pail.u.in /fast-refresh)
          [%skip ~]
        [%done ~]
      ==
    ::
    ++  send-refresh-list
      |=  since=@da
      =/  m  (charm ,pail)
      ^-  form:m
      ;<  history=(list (pair @da path))  bind:m
        %+  scry
          (list (pair @da path))
        /gx/grubbery/history/(scot %da since)/noun
      ~&  >  history+history
      ;<  now=@da  bind:m  get-time
      =/  new-since=@da  ?~(history now p.i.history)
      %+  pure:m  /simple-payload  !>
      %-  manx-response:gen:server 
      (refresher-component:gui new-since (turn history tail))
    --
  ::
  ++  refresher-component
    |=  [since=@da refresh=(list path)]
    ^-  manx
    =.  refresh
      %+  murn  refresh
      |=  =path
      ?.  ?=([%gui %dom *] path)
        ~
      `path
    :: if it's here in our dom folder, update it
    ;div#refresher
      =style  "display: none;"
      =hx-target         "this"
      =hx-get            "/grub/refresher?since={(scow %da since)}"
      =hx-trigger        "load"
      =hx-swap           "outerHTML"
      ;*  =|  id=@
          |-
          ?~  refresh
             ~
          :_  $(refresh t.refresh)
          ;div
            =id            "refresh-fetcher-{(scow %ud id)}"
            =style         "display: none;"
            =hx-target     "#{(make-id i.refresh)}"
            =hx-get        "{(spud i.refresh)}"
            =hx-indicator  "#{(make-id i.refresh)} .loader"
            =hx-swap       "outerHTML"
            =hx-trigger    "click";
    ==
  ::
  ++  session
    =,  grubberyio
    ^-  base:g
    |=  [=bowl:base:g =stud:g =vase]
    =/  m  (charm:base:g ,pail:g)
    ^-  form:m
    ?+    stud  !!
        [%handle-http-request ~]
      =+  !<([request-line:server req=inbound-request:eyre] vase)
      ?+    method.request.req
        %+  pure:m  /simple-payload
        !>((method-not-allowed method.request.req))
        ::
          %'GET'
        ~&  site+site
        ?+    site  !!
          ::
            [%grub %lib *]
          %+  pure:m  /simple-payload
          !>((manx-response:gen:server *manx))
        ==
      ==
    ==
  ::
  ++  base
    =,  grubberyio
    ^-  base:g
    =<
    |=  [=bowl:base:g =stud:g =vase]
    =/  m  (charm:base:g ,pail:g)
    ^-  form:m
    =/  refresher=path  (weld here.bowl /refresher)
    ?+    stud  !!
        [%gui %init ~]
      ;<  ~  bind:m  (eyre-connect /grub here.bowl)
      ;<  ~  bind:m  (overwrite-lib /gui/refresher 'refresher:gui')
      ;<  ~  bind:m  (overwrite-base refresher /dr /gui/refresher `!>(~s5))
      ;<  ~  bind:m  (overwrite-lib /dom/counter 'counter:gui')
      ;<  ~  bind:m
        (overwrite-stem (weld here.bowl /dom/counter) /manx /dom/counter (sy ~[/counter-container/counter]))
      ;<  ~  bind:m  (overwrite-lib /dom/is-even 'is-even:gui')
      ;<  ~  bind:m
        (overwrite-stem (weld here.bowl /dom/is-even) /manx /dom/is-even (sy ~[/counter-container/is-even])) 
      ;<  ~  bind:m  (overwrite-lib /dom/parity 'parity:gui')
      ;<  ~  bind:m
        (overwrite-stem (weld here.bowl /dom/parity) /manx /dom/is-even (sy ~[/counter-container/parity]))
      done
      ::
        [%handle-http-request ~]
      =+  !<([request-line:server req=inbound-request:eyre] vase)
      ?+    method.request.req
        %+  pure:m  /simple-payload
        !>((method-not-allowed method.request.req))
        ::
          %'GET'
        ~&  site+site
        ?+    site
          ;<  =cone:g   bind:m  (peek /)
          %+  pure:m  /simple-payload
          !>((manx-response:gen:server (main-page cone)))
          ::
          ::   [%grub *]
          :: =/  src=@t
          ::   ?:  ?=([@ %eyre @ @ ~] from.bowl)
          ::     i.t.t.from.bowl
          ::   (scot %p our.bowl)
          :: ;<  contents=(list @ta)  bind:m
          ::   (ls (weld here.bowl /[src]))
          :: =/  sessions=(list @ud)
          ::   (sort (turn contents (cury slav %ud)) gth)
          :: ?~  sessions
            :: $-(path [sour=(set path) stem])
            :: create a new session
            :: !!
          :: check if
          :: =/  new=path  
          :: :: redirects to last session and creates one if it 
          :: :: doesn't exist
          :: (redirect:gen:server (spat ))
          :: !!
          ::
            [%grub %make %base ~]
          %+  pure:m  /simple-payload
          !>((manx-response:gen:server (wrap-manx make-base-interface)))
          ::
            [%grub %tree %lib *]
          ;<  g=(unit grub:g)  bind:m  (peek-root-soft t.t.site)
          ;<  ~  bind:m
            ?^(g (pure:(charm ,~) ~) (make-lib t.t.t.site ''))
          ;<  =manx  bind:m  (make-lib-page t.t.t.site)
          %+  pure:m  /simple-payload
          !>((manx-response:gen:server (wrap-manx manx)))
          ::
            [%grub %tree *]
          ;<  =cone:g     bind:m  (peek t.t.site)
          ?~  grub=(~(get of cone) /)
            !!
          =/  cone-con=path
            ?-  -.kind.u.grub
              %base  (weld /bin/gui/con/base base.kind.u.grub)
              %stem  (weld /bin/gui/con/stem stem.kind.u.grub)
            ==
          ;<  s=(unit grub:g)  bind:m
            (peek-root-soft (weld /bin/gui/con/stud stud.u.grub))
          ;<  c=(unit grub:g)  bind:m  (peek-root-soft cone-con)
          =/  stud-manx=manx
            ?~  s
              (vase-to-manx (grab-data u.grub))
            %.  (grab-data u.grub)
            !<($-(^vase manx) (grab-data u.s))
          =/  cone-manx=manx
            ?~  c
              (vase-to-manx (grab-data u.grub))
            (!<($-(cone:g manx) (grab-data u.c)) cone)
          =/  view=(unit @t)  (get-key:kv 'view' args)
          %+  pure:m  /simple-payload  !>
          %-  manx-response:gen:server
          ?^  view
            ?>(?=([~ %cone] view) cone-manx)
          (wrap-manx (cone-interface stud-manx cone-manx))
            
          ::
            [%grub %counter ~]
          ;<  now=@da        bind:m  get-time
          ;<  counter=manx   bind:m  (peek-root-as manx (weld here.bowl /dom/counter))
          ;<  is-even=manx   bind:m  (peek-root-as manx (weld here.bowl /dom/is-even))
          ;<  parity=manx    bind:m  (peek-root-as manx (weld here.bowl /dom/parity))
          %+  pure:m  /simple-payload  !>
          %-  manx-response:gen:server 
          (counter-page now counter is-even parity)
          ::
            [%grub %dom *]
          ;<  =manx   bind:m  (get-dom-manx t.t.site)
          %+  pure:m  /simple-payload  !>
          (manx-response:gen:server manx)
        ==
        ::
          %'POST'
        ?+    site
          =/  action=@t  (need (get-key:kv 'action' args))
          ;<  *  bind:m  (poke /counter /inc !>(~))
          ~&  >>  %poked-counter
          ;<  ~  bind:m
            (send-raw-dart %grub / refresher %bump /fast-refresh !>(~))
          (pure:m /simple-payload !>(two-oh-four))
          ::
            [%grub %make %base ~]
          =/  args=key-value-list:kv  (parse-body:kv body.request.req) 
          =/  =path       (rash (need (get-key:kv 'path' args)) stap)
          =/  =stud:g     (rash (need (get-key:kv 'stud' args)) stap)
          =/  base=^path  (rash (need (get-key:kv 'base' args)) stap)
          ;<  ~  bind:m  (overwrite-base path stud base ~)
          (pure:m /simple-payload !>(two-oh-four))
          ::
            [%grub %make %lib ~]
          =/  args=key-value-list:kv  (parse-body:kv body.request.req) 
          =/  =path    (rash (need (get-key:kv 'path' args)) stap)
          =/  code=@t  (need (get-key:kv 'code' args))
          ;<  ~  bind:m  (overwrite-lib path code)
          ;<  =manx  bind:m  (make-lib-page path)
          %+  pure:m  /simple-payload
          !>((manx-response:gen:server manx))
          ::
            [%grub %poke *]
          ~&  >>>  %receiving-poke-post
          =/  args=key-value-list:kv  (parse-body:kv body.request.req) 
          ;<  =grub:g  bind:m  (peek-root t.t.site)
          ?>  ?=(%base -.kind.grub)
          ;<  p=grub:g  bind:m  (peek-root (weld /bin/gui/con/poke base.kind.grub))
          =/  =pail:g
            (!<($-(key-value-list:kv pail) (grab-data p)) args)
          ;<  *  bind:m  (poke t.t.site pail)
          (pure:m /simple-payload !>(two-oh-four))
          ::
            [%grub %bump *]
          =/  args=key-value-list:kv  (parse-body:kv body.request.req) 
          ;<  =grub:g  bind:m  (peek-root t.t.site)
          ?>  ?=(%base -.kind.grub)
          ;<  b=grub:g  bind:m  (peek-root (weld /bin/gui/con/bump base.kind.grub))
          =/  =pail:g
            (!<($-(key-value-list:kv pail) (grab-data b)) args)
          ;<  ~  bind:m  (bump t.t.site pail)
          (pure:m /simple-payload !>(two-oh-four))
        ==
      ==
    ==
    ::
    |%
    ++  vase-to-manx
      |=  =^vase
      ^-  manx
      ;code:"*{(render-tang-to-marl 80 (sell vase) ~)}"
    ::
    ++  wrap-manx
      |=  =manx
      ^+  manx
      ;html(lang "en")
        ;head
          ;meta(charset "UTF-8");
          ;meta(name "viewport", content "width=device-width, initial-scale=1.0");
          ;title: Grubbery
          ;script(src "https://cdn.tailwindcss.com");
          ;script(src "https://unpkg.com/htmx.org@1.9.4");
          ;script(src "https://code.jquery.com/jquery-3.6.0.min.js");
        ==
        ;body
          ;+  manx
        ==
      ==
    ::
    ++  cone-interface
      |=  [stud=manx cone=manx]
      ^-  manx
      ;div(class "w-screen h-screen mx-auto bg-white shadow-lg rounded-lg flex flex-col")
        ;div(class "flex justify-center p-4 border-b border-gray-300 bg-gray-50")
          ;button
            =id  "studTab"
            =class  "tab-button px-4 py-2 text-gray-700 font-semibold focus:outline-none transition duration-300 border-b-2 border-blue-500 text-blue-500"
            =onclick  "$('#stud-content').show(); $('#cone-content').hide(); $(this).addClass('border-b-2 border-blue-500 text-blue-500').siblings().removeClass('border-b-2 border-blue-500 text-blue-500');"
            Stud
          ==
          ;button
            =id  "coneTab"
            =class  "tab-button px-4 py-2 text-gray-700 font-semibold focus:outline-none transition duration-300"
            =onclick  "$('#cone-content').show(); $('#stud-content').hide(); $(this).addClass('border-b-2 border-blue-500 text-blue-500').siblings().removeClass('border-b-2 border-blue-500 text-blue-500');"
            Cone
          ==
        ==
        ;div#stud-content
          ;+  stud
        ==
        ;div#cone-content.hidden
          ;+  cone
        ==
      ==
    ::
    ++  make-base-interface
      ^-  manx
      ;div(class "max-w-md mx-auto p-6 bg-white shadow-lg rounded-lg mt-10 border border-gray-200")
        ;h2(class "text-2xl font-bold text-gray-700 mb-6 text-center"): Make Base
        ;form
          =class  "space-y-4"
          =hx-post  "/grub/make/base"
          =hx-indicator  "#loading-indicator"
          =hx-swap  "none"
          ;div
            ;label
              =for  "path"
              =class  "block text-gray-700 font-semibold mb-1"
              Path
            ==
            ;input
              =type  "text"
              =id  "path"
              =name  "path"
              =required  ""
              =class  "w-full p-3 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
              ;
            ==
          ==
          ;div
            ;label
              =for  "stud"
              =class  "block text-gray-700 font-semibold mb-1"
              Stud
            ==
            ;input
              =type  "text"
              =id  "stud"
              =name  "stud"
              =required  ""
              =class  "w-full p-3 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
              ;
            ==
          ==
          ;div
            ;label
              =for  "base"
              =class  "block text-gray-700 font-semibold mb-1"
              Base
            ==
            ;input
              =type  "text"
              =id  "base"
              =name  "base"
              =required  ""
              =class  "w-full p-3 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
              ;
            ==
          ==
          ;div(class "flex items-center justify-center mt-6")
            ;button(type "submit", class "bg-blue-500 text-white font-bold py-2 px-6 rounded-lg hover:bg-blue-600 transition duration-300 ease-in-out")
              Make
            ==
            ;div(id "loading-indicator", class "ml-4 htmx-indicator")
              ;svg(class "animate-spin h-6 w-6 text-blue-500", xmlns "http://www.w3.org/2000/svg", fill "none", viewBox "0 0 24 24")
                ;circle(class "opacity-25", cx "12", cy "12", r "10", stroke "currentColor", stroke-width "4");
                ;path(class "opacity-75", fill "currentColor", d "M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4z");
              ==
            ==
          ==
        ==
      ==
    ::
    ++  main-page
      |=  =cone:g
      ^-  manx
      %-  wrap-manx
      ;div.p-4.flex.flex-col.justify-center.items-center.min-h-screen.bg-gray-100
        ;div
          ;*  %+  turn  ~(tap of cone)
              |=  [=path *]
              ;p: {(spud path)}
        ==
      ==
    ::
    ++  make-lib-page
      |=  pat=path
      =/  m  (charm ,manx)
      ^-  form:m
      ;<  [code=@t *]  bind:m  (peek-root-as ,[@t *] [%lib pat])
      ;<  grub=(unit grub:g)  bind:m  (peek-root-soft [%bin pat])
      (pure:m (lib-page pat code grub))
    ::
    ++  lib-page
      |=  [=path code=@t grub=(unit grub:g)]
      ^-  manx
      =/  data=(each vase tang)
        ?~  grub
          |+~[leaf+"no bin; bad dependency"]
        (grab-data-soft u.grub)
      ;div(id (make-id [%lib path]))
        ;div.h-screen.p-4.flex.flex-col.justify-center.items-center.bg-gray-100
          ;div.max-h-screen.h-full.w-full.flex.flex-row
            ;+  (code-result data)
            ;+  (textarea path (trip code))
          ==
        ==
      ==
    ::
    ++  code-result
      |=  data=(each vase tang)
      ^-  manx
      =/  color=tape
        ?:(?=(%| -.data) "red" "green")
      ;div
        =class  "flex-1 flex flex-col w-full bg-{color}-200 p-8 rounded-lg shadow-lg h-full"
        ;div(class "text-xl font-bold mb-4 text-center text-{color}-700")
          ; {?:(?=(%| -.data) "crashed" "compiled")}
        ==
        ;*  ?:  ?=(%| -.data)
              ;=
                ;div(class "bg-{color}-100 p-4 rounded-lg w-full h-full overflow-y-auto")
                  ;code:"*{(render-tang-to-marl 80 p.data)}"
                ==
              ==
            ;=
              ;div(class "bg-{color}-100 p-4 rounded-lg w-full h-full overflow-y-auto")
                ;code:"*{(render-tang-to-marl 80 (sell p.data) ~)}"
              ==
            ==
      ==
    ::
    ++  textarea
      |=  [=path contents=tape]
      ;form
        =hx-post  "/grub/make/lib"
        =hx-swap  "outerHTML"
        =hx-target  "#{(make-id [%lib path])}"
        =class  "flex-1 flex flex-col bg-white p-8 rounded-lg shadow-lg h-full"
        ;label
          =for  "textInput"
          =class  "block text-gray-700 font-bold mb-2"
          ; Enter your text
        ==
        ;input(type "hidden", name "path", value "{(spud path)}");
        ;textarea
          =id  "code"
          =name  "code"
          =rows  "6"
          =cols  "50"
          =class  "font-mono flex-grow h-full mb-3 w-full p-3 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
          =placeholder  "Type something..."
          ; {contents}
        ==
        ;br;
        ;button
          =type   "submit"
          =class  "w-full bg-blue-500 text-white font-bold py-2 px-4 rounded-lg hover:bg-blue-600 transition duration-300 ease-in-out"
          ; Submit
        ==
      ==
    ::
    ++  get-dom-manx
      |=  =path
      =/  m  (charm ,manx)
      ;<  =cone  bind:m  (peek /gui/boot/dom)
      ?~  grub=(~(get of cone) path)
        %-  pure:m
        ;div: No grub at this path.
      =/  res  (mule |.(!<(manx (grab-data u.grub))))
      %-  pure:m
      ?-  -.res
        %&  p.res
        %|  ;div: Bad manx.
      ==
    ::
    ++  counter-page
      |=  [now=@da counter=manx is-even=manx parity=manx]
      ^-  manx
      ;html(lang "en")
        ;head
          ;meta(charset "UTF-8");
          ;meta(name "viewport", content "width=device-width, initial-scale=1.0");
          ;title: Counter App
          ;script(src "https://cdn.tailwindcss.com");
          ;script(src "https://unpkg.com/htmx.org@1.9.4");
        ==
        ;body.p-4.flex.flex-col.justify-center.items-center.min-h-screen.bg-gray-100
          ;+  (refresher-component now ~)
          ;+  counter
          ;+  is-even
          ;+  parity
        ==
      ==
    --
  --
::
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
