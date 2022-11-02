"\ update:
    "\ 1 清理本插件的legacy ,
    "\ 2 left_margin和right_margin默认为空串 不再是2个空格 ,
    "\ 3 把type()相应的数字改为v:t_list等


if exists("g:loaded_easy_align")
    finish
en
let g:loaded_easy_align = 1

let s:cpo_save = &cpo
set cpo&vim

let s:easy_align_delimiters_default = {
    \  ' ': { 'pattern': ' ',  'left_margin': 0, 'right_margin': 0, 'stick_to_left': 0 },
    \  ':': { 'pattern': ':',  'left_margin': 0, 'right_margin': 1, 'stick_to_left': 1 },
    \  ',': { 'pattern': ',',  'left_margin': 0, 'right_margin': 1, 'stick_to_left': 1 },
    \  '|': { 'pattern': '|',  'left_margin': 1, 'right_margin': 1, 'stick_to_left': 0 },
    \  '.': { 'pattern': '\.',  'left_margin': 0, 'right_margin': 0, 'stick_to_left': 0 },
    \
    \  '#': { 'pattern': '#\+', 'delimiter_align': 'l', 'ignore_groups': ['!Comment']  },
    \  '"': { 'pattern': '"\+', 'delimiter_align': 'l', 'ignore_groups': ['!Comment']  },
    \  '&': { 'pattern': '\\\@<!&\|\\\\',
    \                          'left_margin': 1, 'right_margin': 1, 'stick_to_left': 0 },
    \
    \  '=': { 'pattern': '===\|<=>\|\(&&\|||\|<<\|>>\)=\|=\~[#?]\?\|=>\|[:+/*!%^=><&|.?-]\?=[#?]\?',
    \                          'left_margin': 1, 'right_margin': 1, 'stick_to_left': 0 },
    \
    \  '{': { 'pattern': '(\@<!{',
    \                          'left_margin': 1, 'right_margin': 1, 'stick_to_left': 0 },
    \
    \  '}': { 'pattern': '}',  'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 }
\ }

let s:mode_labels = { 'l': '', 'r': '[R]', 'c': '[C]' }


let s:known_options = {
    \ 'left_margin'       :[v:t_string, v:t_number ] ,
    \ 'right_margin'      :[v:t_string, v:t_number ] ,
    \
    \ 'stick_to_left'     :[v:t_number] ,
    \ 'indentation'       :[v:t_string] ,
    \ 'ignore_groups'     :[v:t_list] ,
    \ 'ignore_unmatched'  :[v:t_number] ,
    \ 'delimiter_align'   :[v:t_string] ,
    \ 'ignores'           :[v:t_list] ,
    \ 'filter'            :[v:t_string] ,
    \
    \ 'align'             :[v:t_string] ,
    "\ 作者之前用'mode_sequence', 而非'align', 但现在仍有mode_sequence这个变量,   :help easyalign__option__align
   \ }


let s:option_values = {
\ 'indentation'      : ['shallow', 'deep', 'none', 'keep', -1],
\ 'delimiter_align'  : ['left', 'center', 'right', -1],
\ 'ignore_unmatched' : [0, 1, -1],
\ 'ignore_groups'    : [
                        \ [],
                        \ ['String'],
                        \ ['Comment'],
                        \ ['String', 'Comment'],
                        \ -1,
                    \ ]
\ }

let s:shorthand = {
    \ 'stick_to_left'    : 'stl' ,
    \
    \ 'left_margin'      : 'lm'  ,
    \ 'right_margin'     : 'rm'  ,
    \
    \ 'indentation'      : 'idt' ,
    \
    \ 'ignore_groups'    : 'ig'  ,
    \ 'ignore_unmatched' : 'iu'  ,
    \ 'delimiter_align'  : 'da'  ,
    \ 'ignores'          : 'ig'  ,
    \ 'filter'           : 'f'   ,
    \ 'align'            : 'a'   ,
   \ }

if exists("*strdisplaywidth")
    fun! s:strwidth(str)
        return strdisplaywidth(a:str)
    endf
el
    fun! s:strwidth(str)
        return len(split(a:str, '\zs')) + len(matchstr(a:str, '^\t*')) * (&tabstop - 1)
    endf
en

fun! s:ceil2(v)
    return a:v % 2 == 0 ? a:v : a:v + 1
endf

fun! s:floor2(v)
    return a:v % 2 == 0 ? a:v : a:v - 1
endf

fun! s:highlighted_as(line, col, groups)
    if empty(a:groups) | return 0 | endif
    let hl = synIDattr(synID(a:line, a:col, 0), 'name')
    for grp in a:groups
        if grp[0] == '!'
            if hl !~# grp[1:-1]
                return 1
            en
        elseif hl =~# grp
            return 1
        en
    endfor
    return 0
endf

fun! s:ignored_syntax()
    if has('syntax') && exists('g:syntax_on')
        " Backward-compatibility
        return get(
            \ g:,
            \ 'easy_align_ignore_groups',
            \ get(
                \ g:,
                \ 'easy_align_ignores',
                \ (  get(
                      \ g:,
                      \ 'easy_align_ignore_comment',
                      \ 1,
                     \  ) == 0)
                    \ ?  ['String']
                    \ : ['String', 'Comment'],
               \ ),
           \ )
    el
        return []
    en
endf

fun! s:echon_(tokens)
    " http://vim.wikia.com/wiki/How_to_print_full_screen_width_messages
    let xy = [&ruler, &showcmd]
    try
        set noruler noshowcmd

        let winlen = winwidth(winnr()) - 2
        let len = len(join(map(copy(a:tokens), 'v:val[1]'), ''))
        let ellipsis = len > winlen ? '..' : ''

        echon "\r"
        let yet = 0
        for [hl, msg] in a:tokens
            if empty(msg) | continue | endif
            exe  "echohl ". hl
            let yet += len(msg)
            if yet > winlen - len(ellipsis)
                echon msg[ 0 : (winlen - len(ellipsis) - yet - 1) ] . ellipsis
                break
            el
                echon msg
            en
        endfor
    finally
        echohl None
        let [&ruler, &showcmd] = xy
    endtry
endf

fun! s:echon(l, n, r, d, o, warn)
    let tokens = [
    \ ['Function', s:live ? ':LiveEasyAlign' : ':EasyAlign'],
    \ ['ModeMsg', get(s:mode_labels, a:l, a:l)],
    \ ['None', ' ']]

    if a:r == -1 | call add(tokens, ['Comment', '(']) | endif
    call add(tokens, [a:n =~ '*' ? 'Repeat' : 'Number', a:n])
    call extend(tokens, a:r == 1 ?
    \ [['Delimiter', '/'], ['String', a:d], ['Delimiter', '/']] :
    \ [['Identifier', a:d == ' ' ? '\ ' : (a:d == '\' ? '\\' : a:d)]])
    if a:r == -1 | call extend(tokens, [['Normal', '_'], ['Comment', ')']]) | endif
    call add(tokens, ['Statement', empty(a:o) ? '' : ' '.string(a:o)])
    if !empty(a:warn)
        call add(tokens, ['WarningMsg', ' ('.a:warn.')'])
    en

    call s:echon_(tokens)
    return join(map(tokens, 'v:val[1]'), '')
endf

fun! s:exit(msg)
    call s:echon_([['ErrorMsg', a:msg]])
    throw 'exit'
endf

fun! s:trim_left(str)
    return substitute(a:str, '^\s\+', '', '')
endf

fun! s:trim_right(str)
    return substitute(a:str, '\s\+$', '', '')
endf

fun! s:trim(str)
    return substitute(a:str, '^\s*\(.\{-}\)\s*$', '\1', '')
endf

fun! s:fuzzy_lu(key)
    if has_key(s:known_options, a:key)
        return a:key
    en
    let key = tolower(a:key)

    "\ stick_to_left ?  还是statusline?
    " stl -> ¿^s.*_t.*_l.*¿
    let regexp1 = '^' .key[0]. '.*' .substitute(key[1 : -1], '\(.\)', '_\1.*', 'g')
    let matches = filter( keys(s:known_options), 'v:val =~ regexp1')
    if len(matches) == 1  | return matches[0]  | en

    " stl -> ¿^s.*t.*l.*¿
    let regexp2 = '^' . substitute(substitute(key, '-', '_', 'g'), '\(.\)', '\1.*', 'g')
    let matches = filter( keys(s:known_options), 'v:val =~ regexp2')

    if empty(matches)
        call s:exit("Unknown option key: ". a:key)
    elseif len(matches) == 1
        return matches[0]
    el
        if sort(matches) == ['ignore_groups', 'ignores']
            return 'ignore_groups'
        en
        call s:exit("Ambiguous option key: ". a:key ." (" .join(matches, ', '). ")")
    en
endf

fun! s:shift(modes, cycle)
    let item = remove(a:modes, 0)
    if a:cycle || empty(a:modes)
        call add(a:modes, item)
    en
    return item
endf

fun! s:normalize_options(opts)
    let ret = {}
    for k in keys(a:opts)
        let v = a:opts[k]
        let k = s:fuzzy_lu(k)
        let ret[k] = v
        unlet v
    endfor
    return s:validate_options(ret)
endf

fun! s:compact_options(opts)
    let ret = {}
    for k in keys(a:opts)
        let ret[s:shorthand[k]] = a:opts[k]
    endfor
    return ret
endf

fun! s:validate_options(opts)
    for k in keys(a:opts)
        let v = a:opts[k]
        if index(s:known_options[k], type(v)) == -1
            call s:exit( k . "选项的参数类型错误" .
               \ " , 传入的v是" . string(v) . " , 但其实它的type 要和这个list里的一个相同:" . string(s:known_options[k]) )
        en
        unlet v
    endfor
    return a:opts
endf

fun! s:split_line(line, nth, modes, cycle, fc, lc, pattern, stick_to_left, ignore_unmatched, ignore_groups)
    let mode = ''

    let string = a:lc ?
        \ strpart(getline(a:line), a:fc - 1, a:lc - a:fc + 1) :
        \ strpart(getline(a:line), a:fc - 1)
    let idx     = 0
    let nomagic = match(a:pattern, '\\v') > match(a:pattern, '\C\\[mMV]')
    let pattern = '^.\{-}\s*\zs\('.a:pattern.(nomagic ? ')' : '\)')
    let tokens  = []
    let delims  = []

    " Phase 1: split
    let ignorable = 0
    let token = ''
    let phantom = 0
    while 1
        let matchidx = match(string, pattern, idx)
        " No match
        if matchidx < 0 | break | endif
        let matchend = matchend(string, pattern, idx)
        let spaces = matchstr(
                        \ string                              ,
                        \ '\s' . (a:stick_to_left  ? '*'     : '\{-}') ,
                        \ matchend + (matchidx == matchend)   ,
                        \ )

        " Match, but empty
        if len(spaces) + matchend - idx == 0
            let char = strpart(string, idx, 1)
            if empty(char) | break | endif
            let [match, part, delim] = [char, char, '']
        " Match
        el
            let match = strpart(string, idx, matchend - idx + len(spaces))
            let part  = strpart(string, idx, matchidx - idx)
            let delim = strpart(string, matchidx, matchend - matchidx)
        en

        let ignorable = s:highlighted_as(a:line, idx + len(part) + a:fc, a:ignore_groups)
        if ignorable
            let token .= match
        el
            let [pmode, mode] = [mode, s:shift(a:modes, a:cycle)]
            call add(tokens, token . match)
            call add(delims, delim)
            let token = ''
        en

        let idx += len(match)

        " If the string is non-empty and ends with the delimiter,
        " append an empty token to the list
        if idx == len(string)
            let phantom = 1
            break
        en
    endwhile

    let leftover = token . strpart(string, idx)
    if !empty(leftover)
        let ignorable = s:highlighted_as(a:line, len(string) + a:fc - 1, a:ignore_groups)
        call add(tokens, leftover)
        call add(delims, '')
    elseif phantom
        call add(tokens, '')
        call add(delims, '')
    en
    let [pmode, mode] = [mode, s:shift(a:modes, a:cycle)]

    " Preserve indentation - merge first two tokens
    if len(tokens) > 1 && empty(s:trim_right(tokens[0]))
        let tokens[1] = tokens[0] . tokens[1]
        call remove(tokens, 0)
        call remove(delims, 0)
        let mode = pmode
    en

    " Skip comment line
    if ignorable && len(tokens) == 1 && a:ignore_unmatched
        let tokens = []
        let delims = []
    " Append an empty item to enable right/center alignment of the last token
    " - if the last token is not ignorable or ignorable but not the only token
    elseif a:ignore_unmatched != 1          &&
                \ (mode ==? 'r' || mode ==? 'c')  &&
                \ (!ignorable || len(tokens) > 1) &&
                \ a:nth >= 0 " includes -0
        call add(tokens, '')
        call add(delims, '')
    en

    return [tokens, delims]
endf

fun! s:do_align(todo, modes, all_tokens, all_delims, fl, ll, fc, lc, nth, recur, dict)
    let mode       = a:modes[0]
    let lines      = {}
    let min_indent = -1
    let max = { 'pivot_len2': 0, 'token_len': 0, 'just_len': 0, 'delim_len': 0,
                \ 'indent': 0, 'tokens': 0, 'strip_len': 0 }
    let d = a:dict
    let [f, fx] = s:parse_filter(d.filter)

    " Phase 1
    for line in range(a:fl, a:ll)
        let snip = a:lc > 0 ? getline(line)[a:fc-1 : a:lc-1] : getline(line)
        if f == 1 && snip !~ fx
            continue
        elseif f == -1 && snip =~ fx
            continue
        en

        if !has_key(a:all_tokens, line)
            " Split line into the tokens by the delimiters
            let [tokens, delims] = s:split_line(
                        \ line, a:nth, copy(a:modes), a:recur == 2,
                        \ a:fc, a:lc, d.pattern,
                        \ d.stick_to_left, d.ignore_unmatched, d.ignore_groups)

            " Remember tokens for subsequent recursive calls
            let a:all_tokens[line] = tokens
            let a:all_delims[line] = delims
        el
            let tokens = a:all_tokens[line]
            let delims = a:all_delims[line]
        en

        " Skip empty lines
        if empty(tokens)
            continue
        en

        " Calculate the maximum number of tokens for a line within the range
        let max.tokens = max([max.tokens, len(tokens)])

        if a:nth > 0 " Positive N-th
            if len(tokens) < a:nth
                continue
            en
            let nth = a:nth - 1 " make it 0-based
        el " -0 or Negative N-th
            if a:nth == 0 && mode !=? 'l'
                let nth = len(tokens) - 1
            el
                let nth = len(tokens) + a:nth
            en
            if empty(delims[len(delims) - 1])
                let nth -= 1
            en

            if nth < 0 || nth == len(tokens)
                continue
            en
        en

        let prefix = nth > 0 ? join(tokens[0 : nth - 1], '') : ''
        let delim  = delims[nth]
        let token  = s:trim_right( tokens[nth] )
        let token  = s:trim_right( strpart(token, 0, len(token) - len(s:trim_right(delim))) )
        if empty(delim) && !exists('tokens[nth + 1]') && d.ignore_unmatched
            continue
        en

        let indent = s:strwidth(matchstr(tokens[0], '^\s*'))
        if min_indent < 0 || indent < min_indent
            let min_indent  = indent
        en
        if mode ==? 'c'
            let token .= substitute(matchstr(token, '^\s*'), '\t', repeat(' ', &tabstop), 'g')
        en
        let [pw, tw] = [s:strwidth(prefix), s:strwidth(token)]
        let max.indent    = max([max.indent,    indent])
        let max.token_len = max([max.token_len, tw])
        let max.just_len  = max([max.just_len,  pw + tw])
        let max.delim_len = max([max.delim_len, s:strwidth(delim)])

        if mode ==? 'c'
            let pivot_len2 = pw * 2 + tw
            if max.pivot_len2 < pivot_len2
                let max.pivot_len2 = pivot_len2
            en
            let max.strip_len = max([max.strip_len, s:strwidth(s:trim(token))])
        en
        let lines[line]   = [nth, prefix, token, delim]
    endfor

    " Phase 1-5: indentation handling (only on a:nth == 1)
    if a:nth == 1
        let idt = d.indentation
        if idt ==? 'd'
            let indent = max.indent
        elseif idt ==? 's'
            let indent = min_indent
        elseif idt ==? 'n'
            let indent = 0
        elseif idt !=? 'k'
            call s:exit('Invalid indentation: ' . idt)
        end

        if idt !=? 'k'
            let max.just_len   = 0
            let max.token_len  = 0
            let max.pivot_len2 = 0

            for [line, elems] in items(lines)
                let [nth, prefix, token, delim] = elems

                let tindent = matchstr(token, '^\s*')
                while 1
                    let len = s:strwidth(tindent)
                    if len < indent
                        let tindent .= repeat(' ', indent - len)
                        break
                    elseif len > indent
                        let tindent = tindent[0 : -2]
                    el
                        break
                    en
                endwhile

                let token = tindent . s:trim_left(token)
                if mode ==? 'c'
                    let token = substitute(token, '\s*$', repeat(' ', indent), '')
                en
                let [pw, tw] = [s:strwidth(prefix), s:strwidth(token)]
                let max.token_len = max([max.token_len, tw])
                let max.just_len  = max([max.just_len,  pw + tw])
                if mode ==? 'c'
                    let pivot_len2 = pw * 2 + tw
                    if max.pivot_len2 < pivot_len2
                        let max.pivot_len2 = pivot_len2
                    en
                en

                let lines[line][2] = token
            endfor
        en
    en

    " Phase 2
    for [line, elems] in items(lines)
        let tokens = a:all_tokens[line]
        let delims = a:all_delims[line]
        let [nth, prefix, token, delim] = elems

        " Remove the leading whitespaces of the next token
        if len(tokens) > nth + 1
            let tokens[nth + 1] = s:trim_left(tokens[nth + 1])
        en

        " Pad the token with spaces
        let [pw, tw] = [s:strwidth(prefix), s:strwidth(token)]
        let rpad = ''
        if mode ==? 'l'
            let pad = repeat(' ', max.just_len - pw - tw)
            if d.stick_to_left
                let rpad = pad
            el
                let token = token . pad
            en

        elseif mode ==? 'r'
            let pad = repeat(' ', max.just_len - pw - tw)
            let indent = matchstr(token, '^\s*')
            let token = indent . pad . s:trim_left(token)

        elseif mode ==? 'c'
            let p1  = max.pivot_len2 - (pw * 2 + tw)
            let p2  = max.token_len - tw
            let pf1 = s:floor2(p1)
            if pf1 < p1 | let p2 = s:ceil2(p2)
            el        | let p2 = s:floor2(p2)
            en
            let strip = s:ceil2(max.token_len - max.strip_len) / 2
            let indent = matchstr(token, '^\s*')
            let token = indent. repeat(' ', pf1 / 2) .s:trim_left(token). repeat(' ', p2 / 2)
            let token = substitute(token, repeat(' ', strip) . '$', '', '')

            if d.stick_to_left
                if empty(s:trim_right(token))
                    let center = len(token) / 2
                    let [token, rpad] = [strpart(token, 0, center), strpart(token, center)]
                el
                    let [token, rpad] = [s:trim_right(token), matchstr(token, '\s*$')]
                en
            en
        en
        let tokens[nth] = token

        " Pad the delimiter
        let dpadl = max.delim_len - s:strwidth(delim)
        let da = d.delimiter_align
        if da ==? 'l'
            let [dl, dr] = ['', repeat(' ', dpadl)]
        elseif da ==? 'c'
            let dl = repeat(' ', dpadl / 2)
            let dr = repeat(' ', dpadl - dpadl / 2)
        elseif da ==? 'r'
            let [dl, dr] = [repeat(' ', dpadl), '']
        el
            call s:exit('Invalid delimiter_align: ' . da)
        en

        " Before and after the range (for blockwise visual mode)
        let cline  = getline(line)
        let before = strpart(cline, 0, a:fc - 1)
        let after  = a:lc ? strpart(cline, a:lc) : ''

        " Determine the left and right margin around the delimiter
        let rest   = join(tokens[nth + 1 : -1], '')
        let nomore = empty(rest.after)
        let ml     = (empty(prefix . token) || empty(delim) && nomore) ? '' : d.ml
        let mr     = nomore ? '' : d.mr

        " Adjust indentation of the lines starting with a delimiter
        let lpad = ''
        if nth == 0
            let ipad = repeat(' ', min_indent - s:strwidth(token.ml))
            if mode ==? 'l'
                let token = ipad . token
            el
                let lpad = ipad
            en
        en

        " Align the token
        let aligned = join([lpad, token, ml, dl, delim, dr, mr, rpad], '')
        let tokens[nth] = aligned

        " Update the line
        let a:todo[line] = before.join(tokens, '').after
    endfor

    if a:nth < max.tokens && (a:recur || len(a:modes) > 1)
        call s:shift(a:modes, a:recur == 2)
        return [a:todo, a:modes, a:all_tokens, a:all_delims,
                    \ a:fl, a:ll, a:fc, a:lc, a:nth + 1, a:recur, a:dict]
    en
    return [a:todo]
endf

fun! s:input(str, default, vis)
    if a:vis
        norm! gv
        redraw
        exe  "normal! \<esc>"
    el
        " EasyAlign command can be called without visual selection
        redraw
    en
    let got = input(a:str, a:default)
    return got
endf

fun! s:atoi(str)
    return (a:str =~ '^[0-9]\+$') ? str2nr(a:str) : a:str
endf


fun! s:shift_opts(opts, key, vals)
    let val = s:shift(a:vals, 1)
    if type(val) == v:t_number && val == -1
        call remove(a:opts, a:key)
    el
        let a:opts[a:key] = val
    en
endf

fun! s:interactive(range, modes, n, d, opts, rules, vis, bvis)
    let mode = s:shift(a:modes, 1)
    let n    = a:n
    let d    = a:d
    let ch   = ''
    let opts = s:compact_options(a:opts)
    let vals = deepcopy(s:option_values)
    let regx = 0
    let warn = ''
    let undo = 0

    while 1
        " Live preview
        let rdrw = 0
        if undo
            silent! undo
            let undo = 0
            let rdrw = 1
        en
        if s:live && !empty(d)
            let output = s:process(a:range, mode, n, d, s:normalize_options(opts), regx, a:rules, a:bvis)
            let &undolevels = &undolevels " Break undo block
            call s:update_lines(output.todo)
            let undo = !empty(output.todo)
            let rdrw = 1
        en
        if rdrw
            if a:vis
                norm! gv
            en
            redraw
            if a:vis | execute "normal! \<esc>" | endif
        en
        call s:echon(mode, n, -1, regx ? '/'.d.'/' : d, opts, warn)

        let check = 0
        let warn = ''

        try
            let c = getchar()
        catch /^Vim:Interrupt$/
            let c = 27
        endtry
        let ch = nr2char(c)
        if c == 3 || c == 27 " CTRL-C / ESC
            if undo
                silent! undo
            en
            throw 'exit'
        elseif c == "\<bs>"
            if !empty(d)
                let d = ''
                let regx = 0
            elseif len(n) > 0
                let n = strpart(n, 0, len(n) - 1)
            en
        elseif c == 13 " Enter key
            let mode = s:shift(a:modes, 1)
            if has_key(opts, 'a')
                let opts.a = mode . strpart(opts.a, 1)
            en
        elseif ch == '-'
            if empty(n)      | let n = '-'
            elseif n == '-'  | let n = ''
            el             | let check = 1
            en
        elseif ch == '*'
            if empty(n)      | let n = '*'
            elseif n == '*'  | let n = '**'
            elseif n == '**' | let n = ''
            el             | let check = 1
            en
        elseif empty(d) && ((c == 48 && len(n) > 0) || c > 48 && c <= 57) " Numbers
            if n[0] == '*'   | let check = 1
            el             | let n = n . ch
            end
        elseif ch == "\<C-D>"
            call s:shift_opts(opts, 'da', vals['delimiter_align'])
        elseif ch == "\<C-I>"
            call s:shift_opts(opts, 'idt', vals['indentation'])
        elseif ch == "\<C-L>"
            let lm = s:input("Left margin: ", get(opts, 'lm', ''), a:vis)
            if empty(lm)
                let warn = 'Set to default. Input 0 to remove it'
                silent! call remove(opts, 'lm')
            el
                let opts['lm'] = s:atoi(lm)
            en
        elseif ch == "\<C-R>"
            let rm = s:input("Right margin: ", get(opts, 'rm', ''), a:vis)
            if empty(rm)
                let warn = 'Set to default. Input 0 to remove it'
                silent! call remove(opts, 'rm')
            el
                let opts['rm'] = s:atoi(rm)
            en
        elseif ch == "\<C-U>"
            call s:shift_opts(opts, 'iu', vals['ignore_unmatched'])
        elseif ch == "\<C-G>"
            call s:shift_opts(opts, 'ig', vals['ignore_groups'])
        elseif ch == "\<C-P>"
            if s:live
                if !empty(d)
                    let ch = d
                    break
                el
                    let s:live = 0
                en
            el
                let s:live = 1
            en
        elseif c == "\<Left>"
            let opts['stl'] = 1
            let opts['lm']  = 0
        elseif c == "\<Right>"
            let opts['stl'] = 0
            let opts['lm']  = 1
        elseif c == "\<Down>"
            let opts['lm']  = 0
            let opts['rm']  = 0
        elseif c == "\<Up>"
            silent! call remove(opts, 'stl')
            silent! call remove(opts, 'lm')
            silent! call remove(opts, 'rm')
        elseif ch == "\<C-A>" || ch == "\<C-O>"
            let modes = tolower(s:input("Alignment ([lrc...][[*]*]): ", get(opts, 'a', mode), a:vis))
            if match(modes, '^[lrc]\+\*\{0,2}$') != -1
                let opts['a'] = modes
                let mode      = modes[0]
                while mode != s:shift(a:modes, 1)
                endwhile
            el
                silent! call remove(opts, 'a')
            en
        elseif ch == "\<C-_>" || ch == "\<C-X>"
            if s:live && regx && !empty(d)
                break
            en

            let prompt = 'Regular expression: '
            let ch = s:input(prompt, '', a:vis)
            if !empty(ch) && s:valid_regexp(ch)
                let regx = 1
                let d = ch
                if !s:live | break | endif
            el
                let warn = 'Invalid regular expression: '.ch
            en
        elseif ch == "\<C-F>"
            let f = s:input("Filter (g/../ or v/../): ", get(opts, 'f', ''), a:vis)
            let m = matchlist(f, '^[gv]/\(.\{-}\)/\?$')
            if empty(f)
                silent! call remove(opts, 'f')
            elseif !empty(m) && s:valid_regexp(m[1])
                let opts['f'] = f
            el
                let warn = 'Invalid filter expression'
            en
        elseif ch =~ '[[:print:]]'
            let check = 1
        el
            let warn = 'Invalid character'
        en

        if check
            if empty(d)
                if has_key(a:rules, ch)
                    let d = ch
                    if !s:live
                        if a:vis
                            exe  "normal! gv\<esc>"
                        en
                        break
                    en
                el
                    let warn = 'Unknown delimiter key: '.ch
                en
            el
                if regx
                    let warn = 'Press <CTRL-X> to finish'
                el
                    if d == ch
                        break
                    el
                        let warn = 'Press '''.d.''' again to finish'
                    en
                end
            en
        en
    endwhile
    if s:live
        let copts = call('s:summarize', output.summarize)
        let s:live = 0
        let g:easy_align_last_command = s:echon('', n, regx, d, copts, '')
        let s:live = 1
    end
    return [mode, n, ch, opts, regx]
endf

fun! s:valid_regexp(regexp)
    try
        call matchlist('', a:regexp)
    catch
        return 0
    endtry
    return 1
endf

fun! s:test_regexp(regexp)
    let regexp = empty(a:regexp) ? @/ : a:regexp
    if !s:valid_regexp(regexp)
        call s:exit('Invalid regular expression: '. regexp)
    en
    return regexp
endf

let s:shorthand_regex =
    \ '\s*\%('
    \   .'\(lm\?[0-9]\+\)\|\(rm\?[0-9]\+\)\|\(iu[01]\)\|\(\%(s\%(tl\)\?[01]\)\|[<>]\)\|'
    \   .'\(da\?[clr]\)\|\(\%(ms\?\|a\)[lrc*]\+\)\|\(i\%(dt\)\?[kdsn]\)\|\([gv]/.*/\)\|\(ig\[.*\]\)'
    \ .'\)\+\s*$'

fun! s:parse_shorthand_opts(expr)
    let opts = {}
    let expr = substitute(a:expr, '\s', '', 'g')
    let regex = '^'. s:shorthand_regex

    if empty(expr)
        return opts
    elseif expr !~ regex
        call s:exit("Invalid expression: ". a:expr)
    el
        let match = matchlist(expr, regex)
        for m in filter(match[ 1 : -1 ], '!empty(v:val)')
            for key in ['lm', 'rm', 'l', 'r', 'stl', 's', '<', '>', 'iu', 'da', 'd', 'ms', 'm', 'ig', 'i', 'g', 'v', 'a']
                if stridx(tolower(m), key) == 0
                    let rest = strpart(m, len(key))
                    if key == 'i' | let key = 'idt' | endif
                    if key == 'g' || key == 'v'
                        let rest = key.rest
                        let key = 'f'
                    en

                    if key == 'idt' || index(['d', 'f', 'm', 'a'], key[0]) >= 0
                        let opts[key] = rest
                    elseif key == 'ig'
                        try
                            let arr = eval(rest)
                            if type(arr) == v:t_list
                                let opts[key] = arr
                            el
                                throw 'Not an array'
                            en
                        catch
                            call s:exit("Invalid ignore_groups: ". a:expr)
                        endtry
                    elseif key =~ '[<>]'
                        let opts['stl'] = key == '<'
                    el
                        let opts[key] = str2nr(rest)
                    en
                    break
                en
            endfor
        endfor
    en
    return s:normalize_options(opts)
endf

fun! s:parse_args(args)
    if empty(a:args)
        return ['', '', {}, 0]
    en
    let n    = ''
    let ch   = ''
    let args = a:args
    let cand = ''
    let opts = {}

    " Poor man's option parser
    let idx = 0
    while 1
        let midx = match(args, '\s*{.*}\s*$', idx)
        if midx == -1 | break | endif

        let cand = strpart(args, midx)
        try
            let [l, r, c, k, s, d, n] = ['l', 'r', 'c', 'k', 's', 'd', 'n']
            let [L, R, C, K, S, D, N] = ['l', 'r', 'c', 'k', 's', 'd', 'n']
            let o = eval(cand)
            if type(o) == v:t_dict
                let opts = o
                if args[midx - 1 : midx] == '\ '
                    let midx += 1
                en
                let args = strpart(args, 0, midx)
                break
            en
        catch
            " Ignore
        endtry
        let idx = midx + 1
    endwhile

    " Invalid option dictionary
    if len(substitute(cand, '\s', '', 'g')) > 2 && empty(opts)
        call s:exit("Invalid option: ". cand)
    el
        let opts = s:normalize_options(opts)
    en

    " Shorthand option notation
    let sopts = matchstr(args, s:shorthand_regex)
    if !empty(sopts)
        let args = strpart(args, 0, len(args) - len(sopts))
        let opts = extend(s:parse_shorthand_opts(sopts), opts)
    en

    " Has /Regexp/?
    let matches = matchlist(args, '^\(.\{-}\)\s*/\(.*\)/\s*$')

    " Found regexp
    if !empty(matches)
        return [matches[1], s:test_regexp(matches[2]), opts, 1]
    el
        let tokens = matchlist(args, '^\([1-9][0-9]*\|-[0-9]*\|\*\*\?\)\?\s*\(.\{-}\)\?$')
        " Try swapping n and ch
        let [n, ch] = empty(tokens[2]) ? reverse(tokens[1:2]) : tokens[1:2]

        " Resolving command-line ambiguity
        " '\ ' => ' '
        " '\'  => ' '
        if ch =~ '^\\\s*$'
            let ch = ' '
        " '\\' => '\'
        elseif ch =~ '^\\\\\s*$'
            let ch = '\'
        en

        return [n, ch, opts, 0]
    en
endf

fun! s:parse_filter(f)
    let m = matchlist(a:f, '^\([gv]\)/\(.\{-}\)/\?$')
    if empty(m)
        return [0, '']
    el
        return [m[1] == 'g' ? 1 : -1, m[2]]
    en
endf

fun! s:interactive_modes(bang)
    return get(g:,
        \ (a:bang ? 'easy_align_bang_interactive_modes' : 'easy_align_interactive_modes'),
        \ (a:bang ? ['r', 'l', 'c'] : ['l', 'r', 'c']))
endf

fun! s:alternating_modes(mode)
    return a:mode ==? 'r' ? 'rl' : 'lr'
endf

fun! s:update_lines(todo)
    for [lnum, content] in items(a:todo)
        call setline(lnum, s:trim_right(content))
    endfor
endf

fun! s:parse_nth(n)
    let n = a:n
    let recur = 0
    if n == '*'      | let [nth, recur] = [1, 1]
    elseif n == '**' | let [nth, recur] = [1, 2]
    elseif n == '-'  | let nth = -1
    elseif empty(n)  | let nth = 1
    elseif n == '0' || ( n != '-0' && n != string(str2nr(n)) )
        call s:exit('Invalid N-th parameter: '. n)
    el
        let nth = n
    en
    return [nth, recur]
endf

fun! s:build_dict(delimiters, ch, regexp, opts)
    if a:regexp
        let dict = { 'pattern': a:ch }
    el
        if !has_key(a:delimiters, a:ch)
            call s:exit('Unknown delimiter key: ' . a:ch)
        en
        let dict = copy(a:delimiters[a:ch])
    en

    call extend(dict, a:opts)
    "\ echom "a:opts 是: "   a:opts
    "\ a:opts 是:  {}

    "\ let ml = get(dict, 'left_margin', ' ')
    let ml = get(dict, 'left_margin',   g:easy_align_left_margin )
    let mr = get(dict, 'right_margin',  g:easy_align_right_margin)
    if type(ml) == v:t_number | let ml = repeat(' ', ml) | endif
    if type(mr) == v:t_number | let mr = repeat(' ', mr) | endif
    call extend(
        \ dict,
        \ { 'ml': ml, 'mr': mr },
       \ )

    let dict.pattern = get(dict, 'pattern', a:ch)
    let dict.delimiter_align =
        \ get(
            \ dict,
            \ 'delimiter_align',
            \ get(
                \ g:,
                \ 'easy_align_delimiter_align',
                \ 'r',
               \ ),
           \ )[0]

    let dict.indentation =
        \ get(
            \ dict,
            \ 'indentation',
            \ get(
                \ g:,
                \ 'easy_align_indentation',
                \ 'k',
               \ ),
           \ )[0]

    let dict.stick_to_left =
        \ get(dict, 'stick_to_left', 0)
    let dict.ignore_unmatched =
        \ get(dict, 'ignore_unmatched', get(g:, 'easy_align_ignore_unmatched', 2))
    let dict.ignore_groups =
        \ get(dict, 'ignore_groups', get(dict, 'ignores', s:ignored_syntax()))
    let dict.filter =
        \ get(dict, 'filter', '')
    return dict
endf

fun! s:build_mode_sequence(expr, recur)
    let [expr, recur] = [a:expr, a:recur]
    let suffix = matchstr(a:expr, '\*\+$')
    if suffix == '*'
        let expr = expr[0 : -2]
        let recur = 1
    elseif suffix == '**'
        let expr = expr[0 : -3]
        let recur = 2
    en
    return [tolower(expr), recur]
endf

fun! s:process(range, mode, n, ch, opts, regexp, rules, bvis)
    let [nth, recur] = s:parse_nth((empty(a:n) && exists('g:easy_align_nth')) ? g:easy_align_nth : a:n)
    let dict = s:build_dict(
                    \ a:rules,
                    \ a:ch,
                    \ a:regexp,
                    \ a:opts,
                   \ )
    let [mode_sequence, recur] = s:build_mode_sequence(
        \ get(dict, 'align',    recur == 2
                                    \ ? s:alternating_modes(a:mode)
                                    \ : a:mode),
        \ recur)

    let ve = &virtualedit
    set ve=all
    let args = [
        \ {}, split(mode_sequence, '\zs'),
        \ {}, {}, a:range[0], a:range[1],
        \ a:bvis             ? min([virtcol("'<"), virtcol("'>")]) : 1,
        \ (!recur && a:bvis) ? max([virtcol("'<"), virtcol("'>")]) : 0,
        \ nth, recur, dict ]
    let &ve = ve
    while len(args) > 1
        let args = call('s:do_align', args)
    endwhile

    " todo: lines to update
    " summarize: arguments to s:summarize
    return { 'todo': args[0], 'summarize': [ a:opts, recur, mode_sequence ] }
endf

function s:summarize(opts, recur, mode_sequence)
    let copts = s:compact_options(a:opts)
    let nbmode = s:interactive_modes(0)[0]
    if !has_key(copts, 'a') && (
        \  (a:recur == 2 && s:alternating_modes(nbmode) != a:mode_sequence) ||
        \  (a:recur != 2 && (a:mode_sequence[0] != nbmode || len(a:mode_sequence) > 1))
        \ )
        call extend(copts, { 'a': a:mode_sequence })
    en
    return copts
endf

fun! s:align(bang, live, visualmode, first_line, last_line, expr)
    " Heuristically determine  if the user was in visual mode
    if a:visualmode == 'command'
        let vis  =     a:first_line == line("'<")
              \ && a:last_line  == line("'>")

        let bvis =     vis
              \ && visualmode() == "\<C-V>"

    elseif empty(a:visualmode)
        let vis  = 0
        let bvis = 0
    el
        let vis  = 1
        let bvis = a:visualmode == "\<C-V>"
    endif

    let range = [a:first_line, a:last_line]
    let modes = s:interactive_modes(a:bang)
    let mode  = modes[0]
    "\ echo "modes[0] 是: "   modes[0]
    let s:live = a:live

    let rules = s:easy_align_delimiters_default
    if exists('g:easy_align_delimiters')
        let rules = extend(copy(rules), g:easy_align_delimiters)
    en

    let [n, ch, opts, regexp] = s:parse_args(a:expr)
    "\ echom "s:parse_args(a:expr) 是: "   s:parse_args(a:expr)
        "\ 类似:
            "\ ['1', ' : ', {}, 1]
            "\ ['-1', '=', {}, 0]
            "\ ['-1', ',', {}, 0]

    let bypass_fold = get(g:, 'easy_align_bypass_fold', 0)
    let ofm = &l:foldmethod
    try
        if bypass_fold | let &l:foldmethod = 'manual' | endif

        if empty(n) && empty(ch) || s:live
            let [mode, n, ch, opts, regexp] =
            \s:interactive(
                         \ range,
                         \ copy(modes),
                         \ n,
                         \ ch,
                         \ opts,
                         \ rules,
                         \ vis,
                         \ bvis,
                     \ )
        en

        if !s:live
            let output = s:process(
                             \ range,
                             \ mode,
                             \ n,
                             \ ch,
                             \ s:normalize_options(opts),
                             \ regexp,
                             \ rules,
                             \ bvis,
                          \ )

            call s:update_lines(output.todo)
            let copts = call('s:summarize', output.summarize)
            let g:easy_align_last_command = s:echon('', n, regexp, ch, copts, '')
        en
    finally
        if bypass_fold | let &l:foldmethod = ofm | endif
    endtry
endf

fun! easy_align#align(bang, live, visualmode, expr) range
    try
        call s:align(
            \ a:bang,
            \ a:live,
            \ a:visualmode,
            \ a:firstline,
            \ a:lastline,
            \ a:expr,
           \ )

    catch /^\%(Vim:Interrupt\|exit\)$/
        if empty(a:visualmode)
            echon "\r"
            echon "\r"
        el
            norm! gv
        en
    endtry
endf

let &cpo = s:cpo_save
unlet s:cpo_save

