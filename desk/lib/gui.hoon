/-  g=grubbery
/+  grubberyio, server, html-utils, fi=feather-icons
|%
++  mx  mx:html-utils
++  kv  kv:html-utils
::
++  make-id  |=(p=path (trip (rap 3 (join '_' p))))
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
    (refresher-component new-since (turn history tail))
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
++  base
  =,  grubberyio
  ^-  base:g
  =<
  |=  [=bowl:base:g =stud:g =vase]
  =/  m  (charm:base:g ,pail:g)
  ^-  form:m
  ?+    stud  !!
      [%gui %init ~]
    ;<  ~  bind:m  (eyre-connect /grub here.bowl)
    done
    ::
      [%handle-http-request ~]
    =+  !<([request-line:server req=inbound-request:eyre] vase)
    ~&  >  "received {(trip method.request.req)} request for {(spud site)}!"
    ?+    method.request.req
      %+  pure:m  /simple-payload
      !>((method-not-allowed method.request.req))
      ::
      %'GET'  (do-get [ext site] args)
      ::
        %'POST'
      =/  args=key-value-list:kv  (parse-body:kv body.request.req) 
      =/  get=(unit @t)  (get-key:kv 'get' args)
      ~&  >>  get+get
      ;<  ~  bind:m  (do-post site (delete-key:kv 'get' args))
      ?~  get
        (pure:m /simple-payload !>(two-oh-four))
      (do-get (parse-request-line:server u.get))
    ==
  ==
  ::
  |%
  ++  do-get
    |=  request-line:server
    =/  m  (charm ,pail)
    ^-  form:m
    ?+    site
      ;<  =cone:g   bind:m  (peek /)
      %+  pure:m  /simple-payload
      !>((manx-response:gen:server (main-page cone)))
      ::
        [%grub %make %base ~]
      %+  pure:m  /simple-payload
      !>((manx-response:gen:server (wrap-manx make-base-interface)))
      ::
        [%grub %main ~]
      ;<  =cone:g  bind:m  (peek /)
      %+  pure:m  /simple-payload
      !>((manx-response:gen:server (wrap-manx (main cone))))
      ::
        [%grub %search-bar *]
      %+  pure:m  /simple-payload
      !>((manx-response:gen:server (search-bar t.t.site)))
      ::
        [%grub %tree %lib *]
      ;<  g=(unit grub:g)  bind:m  (peek-root-soft t.t.site)
      ;<  ~  bind:m
        ?^(g (pure:(charm ,~) ~) (make-lib t.t.t.site ''))
      ;<  =manx  bind:m  (make-lib-page t.t.t.site)
      =/  raw=(unit @t)  (get-key:kv 'raw' args)
      ?^  raw
        ?>(?=([~ %true] raw) (give-manx-response manx))
      (give-manx-response (wrap-manx manx))
      ::
        [%grub %tree *]
      ;<  =manx  bind:m  (grub-tree t.t.site)
      (give-manx-response manx)
      ::
        [%grub %view %lib *]
      ;<  g=(unit grub:g)  bind:m  (peek-root-soft t.t.site)
      ;<  ~  bind:m
        ?^(g (pure:(charm ,~) ~) (make-lib t.t.t.site ''))
      ;<  =manx  bind:m  (make-lib-page t.t.t.site)
      (give-manx-response manx)
      ::
        [%grub %view %both *]
      ;<  =manx  bind:m  (grub-view-both t.t.t.site)
      (give-manx-response manx)
      ::
        [%grub %view %stud *]
      ;<  =manx  bind:m  (grub-view-stud t.t.t.site)
      (give-manx-response manx)
      ::
        [%grub %view %cone *]
      ;<  =manx  bind:m  (grub-view-cone t.t.t.site)
      (give-manx-response manx)
      ::
        [%grub %code *]
      ;<  =manx  bind:m  (grub-code t.t.site)
      (give-manx-response manx)
      ::
        [%grub %gui %con *]
      ;<  =manx  bind:m  (grub-gui-con t.t.t.site)
      (give-manx-response manx)
      ::
        [%grub %dom *]
      ;<  =manx   bind:m  (get-dom-manx t.t.site)
      %+  pure:m  /simple-payload  !>
      (manx-response:gen:server manx)
    ==
  ::
  ++  do-post
    |=  [site=path args=key-value-list:kv]
    =/  m  (charm ,~)
    ^-  form:m
    ?+    site  !!
      [%grub %get ~]  (pure:m ~)
      ::
        [%grub %make %base ~]
      =/  =path       (rash (need (get-key:kv 'path' args)) stap)
      =/  =stud:g     (rash (need (get-key:kv 'stud' args)) stap)
      =/  base=^path  (rash (need (get-key:kv 'base' args)) stap)
      ;<  ~  bind:m  (overwrite-base path stud base ~)
      (pure:m ~)
      ::
        [%grub %make %lib ~]
      =/  =path    (rash (need (get-key:kv 'path' args)) stap)
      =/  code=@t  (need (get-key:kv 'code' args))
      ;<  ~  bind:m  (overwrite-lib path code)
      (pure:m ~)
      ::
        [%grub %poke *]
      ~&  >>>  %receiving-poke-post
      ;<  =grub:g  bind:m  (peek-root t.t.site)
      ?>  ?=(%base -.kind.grub)
      ;<  p=grub:g  bind:m  (peek-root (weld /bin/gui/con/poke base.kind.grub))
      =/  =pail:g
        (!<($-(key-value-list:kv pail) (grab-data p)) args)
      ;<  *  bind:m  (poke t.t.site pail)
      (pure:m ~)
      ::
        [%grub %bump *]
      ;<  =grub:g  bind:m  (peek-root t.t.site)
      ?>  ?=(%base -.kind.grub)
      ;<  b=grub:g  bind:m  (peek-root (weld /bin/gui/con/bump base.kind.grub))
      =/  =pail:g
        (!<($-(key-value-list:kv pail) (grab-data b)) args)
      ;<  ~  bind:m  (bump t.t.site pail)
      (pure:m ~)
    ==
    
  ++  vase-to-manx
    |=  =^vase
    ^-  manx
    ;code.flex-grow:"*{(render-tang-to-marl 80 (sell vase) ~)}"
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
  ++  main
    |=  =cone:g
    ^-  manx
    ;div.flex.flex-col.h-screen.w-screen
      ;+  (navbar /)
      ;div.h-full.w-full.flex.flex-row.bg-gray-500.overflow-hidden
        ;div
          =id  "tree-tab"
          =class  "w-1/3 overflow-auto text-white font-mono font-bold"
          ;+  (cone-navigator / cone)
        ==
        ;div#display.h-full.w-full.flex.flex-col.bg-gray-100.items-center.justify-center.overflow-auto;
      ==
    ==
  ::
  ++  navbar
    |=  =path
    ^-  manx
    ;div.h-12.w-full.flex.flex-row.items-center.justify-start.text-white.font-mono.font-bold.bg-blue-500
      ;button
        =class  "px-4 py-2 hover:bg-blue-200"
        =onclick  "$('#tree-tab').toggleClass('hidden');"
        ;+  (make:fi %menu)
      ==
      ;+  (search-bar path)
    ==
  ::
  ++  search-bar
    |=  =path
    ^-  manx
    ;div(id "search-bar", class "h-full w-full flex flex-row")
      ;form#search
        =class  "hidden m-1 flex-grow flex flex-row"
        =hx-post  "/grub/get"
        =hx-swap  "outerHTML"
        =hx-target  "#search-bar"
        =onsubmit  "document.getElementById('search-in').value = '/grub/search-bar' + document.getElementById('search-in').value;"
        =hx-on-htmx-after-request  "$('#search').addClass('hidden'); $('#crumbs').removeClass('hidden');"
        ;input#search-in
          =class  "p-2 bg-blue-200 flex-grow font-mono text-gray-600 focus:outline-none focus:ring-1 focus:ring-blue-500"
          =type  "text"
          =required  ""
          =name  "get"
          =placeholder  ""
          =pattern  "^(/)?([a-zA-Z0-9_\\-\\~\\.]+(/)?)*$"
          =spellcheck  "false"
          =autocomplete  "off"
          =onfocusout  "$('#search').addClass('hidden'); $('#crumbs').removeClass('hidden');"
          =value  ?:(=(~ path) "/" "{(spud path)}/")
          ;
        ==
        ;div(class "font-mono p-2 bg-blue-400")
          ;+  (make:fi %search)
        ==
      ==
      ;div(id "crumbs", class "m-1 flex-grow flex flex-row items-center justify-center")
        ;*  %+  turn  (gulf 0 (lent path))
            |=  i=@ud
            ^-  manx
            ;div
              =hx-get  "/grub/search-bar{(spud (scag i path))}"
              =hx-target  "#search-bar"
              =hx-swap  "outerHTML"
              =hx-trigger  "click"
              =class  "cursor-pointer"
              ; {?:(=(0 i) "/" "{(trip (snag (dec i) path))}/")}
            ==
        ;button
          =class  "font-mono h-full w-full flex flex-row p-2 hover:bg-blue-200 justify-between"
          =onclick  "$('#crumbs').addClass('hidden'); $('#search').removeClass('hidden'); $('#search-in').focus().get(0).setSelectionRange(999,999);"
          ;div.hidden
            =hx-get  "/grub/tree{(spud path)}"
            =hx-target  "#display"
            =hx-swap  "innerHTML"
            =hx-trigger  "load"
            =hx-indicator  "#loading-indicator"
            ;
          ==
          ;div
            =id  "loading-indicator"
            =class  "htmx-indicator p-2"
            ;+  (pac:~(at mx (make:fi %loader)) "animate-spin")
          ==
          ;+  (make:fi %search)
        ==
      ==
    ==
  ::
  ++  cone-navigator
    |=  [=path =cone:g]
    ^-  manx
    ;div.pl-6
      ;div
        =class  "flex flex-row px-1 py-2 bg-blue-500 hover:bg-blue-200"
        ;*  ?:  =(~ dir.cone)
              ~
            ;=
              ;button.chevron-right.hidden
                =onclick  "$(this).addClass('hidden').siblings('.chevron-down').removeClass('hidden').parent().next().removeClass('hidden');"
                ;+  (make:fi %chevron-right)
              ==
              ;button.chevron-down
                =onclick  "$(this).addClass('hidden').siblings('.chevron-right').removeClass('hidden').parent().next().addClass('hidden');"
                ;+  (make:fi %chevron-down)
              ==
            ==
        ;div
          =hx-get  "/grub/search-bar{(spud path)}"
          =hx-target  "#search-bar"
          =hx-swap  "outerHTML"
          =hx-trigger  "click"
          =class  "cursor-pointer"
          ; {?~(path "/" (trip (rear path)))}
        ==
      ==
      ;div.children.flex.flex-col
        ;*  %+  turn  ~(tap by dir.cone)
            |=  [name=@ta =cone:g]
            (cone-navigator (weld path /[name]) cone)
      ==
    ==
  ::
  ++  grub-tree
    |=  =path
    =/  m  (charm ,manx)
    ^-  form:m
    %-  pure:m
    ^-  manx
    ;div(class "w-full h-full mx-auto bg-white shadow-lg rounded-lg flex flex-col")
      ;div(class "flex justify-center p-4 border-b border-gray-300 bg-gray-50")
        ;button
          =id  "viewTab"
          =class  "tab-button px-4 py-2 text-gray-700 font-semibold focus:outline-none transition duration-300 border-b-2 border-blue-500 text-blue-500"
          =onclick  "$(this).addClass('border-b-2 border-blue-500 text-blue-500').siblings().removeClass('border-b-2 border-blue-500 text-blue-500');"
          =hx-get  "/grub/view/both{(spud path)}"
          =hx-target  "#inner-display"
          =hx-swap  "innerHTML"
          =hx-trigger  "click, load"
          ; View
        ==
        ;button
          =id  "codeTab"
          =class  "tab-button px-4 py-2 text-gray-700 font-semibold focus:outline-none transition duration-300"
          =onclick  "$(this).addClass('border-b-2 border-blue-500 text-blue-500').siblings().removeClass('border-b-2 border-blue-500 text-blue-500');"
          =hx-get  "/grub/code{(spud path)}"
          =hx-target  "#inner-display"
          =hx-swap  "innerHTML"
          ; Code
        ==
        ;button
          =id  "conversionsTab"
          =class  "tab-button px-4 py-2 text-gray-700 font-semibold focus:outline-none transition duration-300"
          =onclick  "$(this).addClass('border-b-2 border-blue-500 text-blue-500').siblings().removeClass('border-b-2 border-blue-500 text-blue-500');"
          =hx-get  "/grub/gui/con{(spud path)}"
          =hx-target  "#inner-display"
          =hx-swap  "innerHTML"
          ; Conversions
        ==
      ==
      ;div#inner-display.h-full.w-full.flex.flex-col.bg-gray-300.items-center.justify-center.overflow-auto;
    ==
  ::
  ++  grub-code
    |=  =path
    =/  m  (charm ,manx)
    ^-  form:m
    ;<  =cone:g  bind:m  (peek path)
    =/  =grub:g  (need (~(get of cone) /))
    %-  pure:m
    ^-  manx
    ;div(class "w-full h-full mx-auto bg-white shadow-lg rounded-lg flex flex-col")
      ;div(class "flex justify-center p-4 border-b border-gray-300 bg-gray-50")
        ;button
          =id  "viewTab"
          =class  "tab-button px-4 py-2 text-gray-700 font-semibold focus:outline-none transition duration-300 border-b-2 border-blue-500 text-blue-500"
          =onclick  "$('#stud-path').show(); $('#grub-path').hide(); $(this).addClass('border-b-2 border-blue-500 text-blue-500').siblings().removeClass('border-b-2 border-blue-500 text-blue-500');"
          =hx-get  "/grub/view/lib/stud{(spud stud.grub)}"
          =hx-target  "#code-display"
          =hx-swap  "innerHTML"
          =hx-trigger  "click, load"
          ; Stud
        ==
        ;button
          =id  "codeTab"
          =class  "tab-button px-4 py-2 text-gray-700 font-semibold focus:outline-none transition duration-300"
          =onclick  "$('#grub-path').show(); $('#stud-path').hide(); $(this).addClass('border-b-2 border-blue-500 text-blue-500').siblings().removeClass('border-b-2 border-blue-500 text-blue-500');"
          =hx-get  "/grub/view/lib/{?:(?=(%base -.kind.grub) "base{(spud base.kind.grub)}" "stem{(spud stem.kind.grub)}")}"
          =hx-target  "#code-display"
          =hx-swap  "innerHTML"
          ;+  ;/
          ?:  ?=(%base -.kind.grub)
            "Base"
          "Stem"
        ==
      ==
      ;div#stud-path.w-full.p-4.bg-gray-100.rounded-lg.shadow-md.text-center.text-gray-700.text-lg.font-semibold
        ; {(spud stud.grub)}
      ==
      ;div#grub-path.hidden.w-full.p-4.bg-gray-100.rounded-lg.shadow-md.text-center.text-gray-700.text-lg.font-semibold
        ;+  ;/
        ?:  ?=(%base -.kind.grub)
          (spud base.kind.grub)
        (spud stem.kind.grub)
      ==
      ;div#code-display.h-full.w-full.flex.flex-col.bg-gray-300.items-center.justify-center.overflow-auto;
    ==
  ::
  ++  grub-gui-con
    |=  =path
    =/  m  (charm ,manx)
    ^-  form:m
    ;<  =cone:g  bind:m  (peek path)
    =/  =grub:g  (need (~(get of cone) /))
    %-  pure:m
    ^-  manx
    ;div(class "w-full h-full mx-auto bg-white shadow-lg rounded-lg flex flex-col")
      ;div(class "flex justify-center p-4 border-b border-gray-300 bg-gray-50")
        ;button
          =id  "viewTab"
          =class  "tab-button px-4 py-2 text-gray-700 font-semibold focus:outline-none transition duration-300 border-b-2 border-blue-500 text-blue-500"
          =onclick  "$('#stud-path').show().siblings().hide(); $(this).addClass('border-b-2 border-blue-500 text-blue-500').siblings().removeClass('border-b-2 border-blue-500 text-blue-500');"
          =hx-get  "/grub/view/lib/gui/con/stud{(spud stud.grub)}"
          =hx-target  "#gui-con-display"
          =hx-swap  "innerHTML"
          =hx-trigger  "click, load"
          ; Stud
        ==
        ;button
          =id  "codeTab"
          =class  "tab-button px-4 py-2 text-gray-700 font-semibold focus:outline-none transition duration-300"
          =onclick  "$('#cone-path').show().siblings().hide(); $(this).addClass('border-b-2 border-blue-500 text-blue-500').siblings().removeClass('border-b-2 border-blue-500 text-blue-500');"
          =hx-get  "/grub/view/lib/gui/con/{?:(?=(%base -.kind.grub) "base{(spud base.kind.grub)}" "stem{(spud stem.kind.grub)}")}"
          =hx-target  "#gui-con-display"
          =hx-swap  "innerHTML"
          ; Cone
        ==
        ;*  ?.  ?=(%base -.kind.grub)
              ~
            ;=
              ;button
                =id  "codeTab"
                =class  "tab-button px-4 py-2 text-gray-700 font-semibold focus:outline-none transition duration-300"
                =onclick  "$('#cone-path').show().siblings().hide(); $(this).addClass('border-b-2 border-blue-500 text-blue-500').siblings().removeClass('border-b-2 border-blue-500 text-blue-500');"
                =hx-get  "/grub/view/lib/gui/con/poke{(spud base.kind.grub)}"
                =hx-target  "#gui-con-display"
                =hx-swap  "innerHTML"
                ; Poke
              ==
              ;button
                =id  "codeTab"
                =class  "tab-button px-4 py-2 text-gray-700 font-semibold focus:outline-none transition duration-300"
                =onclick  "$('#cone-path').show().siblings().hide(); $(this).addClass('border-b-2 border-blue-500 text-blue-500').siblings().removeClass('border-b-2 border-blue-500 text-blue-500');"
                =hx-get  "/grub/view/lib/gui/con/bump{(spud base.kind.grub)}"
                =hx-target  "#gui-con-display"
                =hx-swap  "innerHTML"
                ; Bump
              ==
            ==
      ==
      ;div.flex.flex-col.w-full.p-4.bg-gray-100.rounded-lg.shadow-md.text-center.text-gray-700.text-lg.font-semibold
        ;div#stud-path: {(spud stud.grub)}
        ;div#cone-path.hidden
          ;+  ;/
          ?:  ?=(%base -.kind.grub)
            (spud base.kind.grub)
          (spud stem.kind.grub)
        ==
      ==
      ;div#gui-con-display.h-full.w-full.flex.flex-col.bg-gray-300.items-center.justify-center.overflow-auto;
    ==
  ::
  ++  grub-view-stud
    |=  =path
    =/  m  (charm ,manx)
    ^-  form:m
    ;<  =cone:g  bind:m  (peek path)
    =/  =grub:g  (need (~(get of cone) /))
    ;<  s=(unit grub:g)  bind:m
      (peek-root-soft (weld /bin/gui/con/stud stud.grub))
    %-  pure:m
    ?~  s
      (vase-to-manx (grab-data grub))
    %.  (grab-data grub)
    !<($-(^vase manx) (grab-data u.s))
  ::
  ++  grub-view-cone
    |=  =path
    =/  m  (charm ,manx)
    ^-  form:m
    ;<  =cone:g  bind:m  (peek path)
    =/  =grub:g  (need (~(get of cone) /))
    =/  con-path=^path
      ?-  -.kind.grub
        %base  (weld /bin/gui/con/base base.kind.grub)
        %stem  (weld /bin/gui/con/stem stem.kind.grub)
      ==
    ;<  c=(unit grub:g)  bind:m  (peek-root-soft con-path)
    %-  pure:m
    ?~  c
      (vase-to-manx (grab-data grub))
    (!<($-([^path cone:g] manx) (grab-data u.c)) path cone)
  ::
  ++  grub-view-both
    |=  =path
    =/  m  (charm ,manx)
    ^-  form:m
    %-  pure:m
    ;div(class "w-full h-full mx-auto bg-white shadow-lg rounded-lg flex flex-col")
      ;div(class "flex justify-center p-4 border-b border-gray-300 bg-gray-50")
        ;button
          =id  "studTab"
          =class  "tab-button px-4 py-2 text-gray-700 font-semibold focus:outline-none transition duration-300 border-b-2 border-blue-500 text-blue-500"
          =onclick  "$(this).addClass('border-b-2 border-blue-500 text-blue-500').siblings().removeClass('border-b-2 border-blue-500 text-blue-500');"
          =hx-get  "/grub/view/stud{(spud path)}"
          =hx-target  "#view-display"
          =hx-swap  "innerHTML"
          =hx-trigger  "click, load"
          ; Stud
        ==
        ;button
          =id  "coneTab"
          =class  "tab-button px-4 py-2 text-gray-700 font-semibold focus:outline-none transition duration-300"
          =onclick  "$(this).addClass('border-b-2 border-blue-500 text-blue-500').siblings().removeClass('border-b-2 border-blue-500 text-blue-500');"
          =hx-get  "/grub/view/cone{(spud path)}"
          =hx-target  "#view-display"
          =hx-swap  "innerHTML"
          ; Cone
        ==
      ==
      ;div#view-display.h-full.w-full.flex.flex-col.bg-gray-300.items-center.justify-center.overflow-auto;
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
    ;div
      =id  (make-id [%lib path])
      =class  "h-full w-full p-4 flex-grow flex flex-row justify-center items-center bg-gray-100"
      ;+  (code-result data)
      ;+  (textarea path (trip code))
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
    ^-  manx
    ;div(class "flex-1 flex flex-col w-full bg-white p-8 rounded-lg shadow-lg h-full")
      ;form(class "flex-grow flex flex-col")
        =hx-post  "/grub/make/lib"
        =hx-trigger  "submit"
        =hx-swap  "outerHTML"
        =hx-target  "#{(make-id %lib path)}"
        ;label
          =for  "textInput"
          =class  "block text-gray-700 font-bold mb-2"
          ; Enter your text
        ==
        ;input(type "hidden", name "get", value "/grub/tree/lib{(spud path)}?raw=true");
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