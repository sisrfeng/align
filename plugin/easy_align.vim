if exists("g:loaded_easy_align_plugin")
    finish
en
let g:loaded_easy_align_plugin = 1

com!  -nargs=* -range -bang EasyAlign
    \ <line1>,<line2>call easy_align#align(<bang>0, 0, 'command', <q-args>)

com!  -nargs=* -range -bang LiveEasyAlign
    \ <line1>,<line2>call easy_align#align(<bang>0, 1, 'command', <q-args>)

let s:last_command = 'EasyAlign'

fun! s:abs(v)
    return a:v >= 0
        \ ? a:v
        \ : - a:v
endf

fun! s:remember_visual(mode)
    let s:last_visual = [
        \ a:mode,
        \ s:abs(  line("'>") - line("'<")),
              \s:abs(col("'>") - col("'<")
            \ ),
       \ ]
endf

fun! s:repeat_visual()
    let [mode, ldiff, cdiff] = s:last_visual
    let cmd = 'normal! ' . mode
    if ldiff > 0
        let cmd .= ldiff . 'j'
    en

    let ve_save = &virtualedit
    try
        if mode == "\<C-V>"
            if cdiff > 0
                let cmd .= cdiff . 'l'
            en
            set virtualedit+=block
        en
        exe  cmd.":\<C-r>=g:easy_align_last_command\<Enter>\<Enter>"
        call s:set_repeat()
    finally
        if ve_save != &virtualedit
            let &virtualedit = ve_save
        en
    endtry
endf

fun! s:repeat_in_visual()
    if exists('g:easy_align_last_command')
        call s:remember_visual(visualmode())
        call s:repeat_visual()
    en
endf

fun! s:set_repeat()
    silent! call repeat#set("\<Plug>(EasyAlignRepeat)")
endf

fun! s:generic_easy_align_op(type, vmode, live)
    if !&modifiable
        if a:vmode
            norm! gv
        en
        return
    en
    let sel_save = &selection
    let &selection = "inclusive"

    if a:vmode
        let vmode = a:type
        let [l1, l2] = ["'<", "'>"]
        call s:remember_visual(vmode)
    el
        let vmode = ''
        let [l1, l2] = [line("'["), line("']")]
        unlet! s:last_visual
    en

    try
        let range = l1.','.l2
        if get(g:, 'easy_align_need_repeat', 0)
            exe  range . g:easy_align_last_command
        el
            exe  range . "call easy_align#align(0, a:live, vmode, '')"
        end
        call s:set_repeat()
    finally
        let &selection = sel_save
    endtry
endf

fun! s:easy_align_op(type, ...)
    call s:generic_easy_align_op(a:type, a:0, 0)
endf

fun! s:live_easy_align_op(type, ...)
    call s:generic_easy_align_op(a:type, a:0, 1)
endf

fun! s:easy_align_repeat()
    if exists('s:last_visual')
        call s:repeat_visual()
    el
        try
            let g:easy_align_need_repeat = 1
            norm! .
        finally
            unlet! g:easy_align_need_repeat
        endtry
    en
endf

nno  <silent> <Plug>(EasyAlign) :set opfunc=<SID>easy_align_op<Enter>g@
vno  <silent> <Plug>(EasyAlign) :<C-U>call <SID>easy_align_op(visualmode(), 1)<Enter>
nno  <silent> <Plug>(LiveEasyAlign) :set opfunc=<SID>live_easy_align_op<Enter>g@
vno  <silent> <Plug>(LiveEasyAlign) :<C-U>call <SID>live_easy_align_op(visualmode(), 1)<Enter>

" vim-repeat support
nno  <silent> <Plug>(EasyAlignRepeat) :call <SID>easy_align_repeat()<Enter>
vno  <silent> <Plug>(EasyAlignRepeat) :<C-U>call <SID>repeat_in_visual()<Enter>

" Backward-compatibility (deprecated)
nno  <silent> <Plug>(EasyAlignOperator) :set opfunc=<SID>easy_align_op<Enter>g@

