######################################
#
#  Python RegExp Tools
#  
#  By Runsun Pan (runsun at gmail dot com) 2016/2
#  python version: 2.7.11 
#  (Seems to work fine in 3.6.2)
#
#  Run this file directly for doctests
######################################

import re, doctest, collections

#. re func : c, nc
def _c(txt): return r'('+txt+r')'     ## captured group
def _nc(txt): return r'(?:'+txt+r')'  ## non-captured group
#. re func : or  
def _or_items( data, itemcap=''):
  '''
    >>> _or_items( ('abc','def') )
    '(abc|def)'
    
    >>> _or_items( ('abc','def'), 'c' )
    '((abc)|(def))'
    
    >>> _or_items( ('abc','def'), 'nc' )
    '((?:abc)|(?:def))'
    
  '''
    
  if itemcap == 'c': data=[ _c(x) for x in data ]
  elif itemcap=='nc': data=[ _nc(x) for x in data ]
  return  _c( r'|'.join(data) )
def _or(*args): 
  data = (type(args[0]) in (list,tuple)) and args[0] or args
  return _or_items( data, itemcap='')
    
def _or_c(*args): 
  data = (type(args[0]) in (list,tuple)) and args[0] or args
  return _or_items( data, itemcap='c')

def _or_nc(*args): 
  data = (type(args[0]) in (list,tuple)) and args[0] or args
  return _or_items( data, itemcap='nc')

def _nc_or_items( data, itemcap=''):
  if itemcap == 'c': data=[ _c(x) for x in data ]
  elif itemcap=='nc': data=[ _nc(x) for x in data ]
  return  _nc( r'|'.join(data) )    
  
def _nc_or(*args): 
  data = (type(args[0]) in (list,tuple)) and args[0] or args
  return _nc_or_items( data, itemcap='')
    
def _nc_or_c(*args): 
  data = (type(args[0]) in (list,tuple)) and args[0] or args
  return _nc_or_items( data, itemcap='c')

def _nc_or_nc(*args): 
  data = (type(args[0]) in (list,tuple)) and args[0] or args
  return _nc_or_items( data, itemcap='nc')
            
def test_re_or():
    '''                     
    >>> _or( ('abc','def'))
    '(abc|def)'
    >>> _or( 'abc','def')
    '(abc|def)'
    
    >>> _or_c( 'abc','def')
    '((abc)|(def))'
    >>> _or_c( ('abc','def'))
    '((abc)|(def))'
                           
    >>> _or_c( 'abc','def')
    '((abc)|(def))'
    >>> _or_nc( ('abc','def'))
    '((?:abc)|(?:def))'
       
    >>> _nc_or( ('abc','def'))
    '(?:abc|def)'
    >>> _nc_or( 'abc','def')
    '(?:abc|def)'
    >>> _nc_or_c( 'abc','def')
    '(?:(abc)|(def))'
    >>> _nc_or_c( ('abc','def'))
    '(?:(abc)|(def))'
                           
    >>> _nc_or_c( 'abc','def')
    '(?:(abc)|(def))'
    >>> _nc_or_nc( ('abc','def'))
    '(?:(?:abc)|(?:def))'
    
    '''
#. re func : repeats 
def _rep( data, count=r'*', counts=None, cap=''):
  '''
    >>> _rep('abc') 
    'abc*'
    
    >>> _rep('abc', '+')
    'abc+'
    
    >>> _rep('abc', count='?', cap='c')
    '(abc)?'
    
    >>> _rep('abc', count='*', cap='nc')
    '(?:abc)*'
    
    >>> _rep('abc', counts='2,3', cap='nc')
    '(?:abc){2,3}'
     
  '''
  if counts:
    return { 'nc' : r'(?:' + data+ r'){'+counts+ r'}'
           , 'c'  : r'(' + data+ r'){'+counts+ r'}'
           , ''   : data+ r'){'+counts+ r'}'  
           }[cap]  
  else:
    return { 'nc' : r'(?:' + data+ ')'+count
           , 'c'  : r'(' + data+ ')'+count  
           , ''   : data+ count 
           }[cap]         
           
def _nc_rep( data, count=r'*', counts=None):
  return _rep( data, count=count, counts=counts, cap = 'nc')
  
def _01(data):  return _rep(data, count=r'?', cap='c' )
def _0m(data):  return _rep(data, count=r'*', cap='c' )
def _1m(data):  
  '''    
  >>> _1m('abc')
  '(abc)+'
  '''
  return _rep(data, count=r'+', cap='c' )

def _nc_01(data): return _rep(data, count=r'?', cap='nc' )
def _nc_0m(data): return _rep(data, count=r'*', cap='nc' )
def _nc_1m(data): 
  '''    
  >>> _nc_1m('abc')
  '(?:abc)+'
  
  '''
  return _rep(data, count=r'+', cap='nc' )
def _0ms(): return r'\s*?'
def _1ms(): return r'\s+?'
  
#. re func : af/fb            
def _af(x, data): return r'(?<=%s)%s'%(x,data)    # after pre
def _naf(x, data): return r'(?<!%s)%s'%(x,data)
def _fb(data, x): return r'%s(?=%s)'%(data,x)    # before post
def _nfb( data, x): return r'%s(?!%s)'%(data,x)  
def _af_fb( x,data,x2): return r'(?<=%s)%s(?=%s)'%(x,data,x2)
def _af_nfb( x,data,x2): return r'(?<=%s)%s(?!%s)'%(x,data,x2)
def _naf_fb(x,data,x2): return r'(?<!%s)%s(?=%s)'%(x,data,x2)
def _naf_nfb(x,data,x2): return r'(?<!%s)%s(?!%s)'%(x,data,x2)

def test_affb():
  r'''
    >>> p= _af_fb('"','.*?', '"')
    >>> p   
    '(?<=").*?(?=")'
    >>> re.compile(p).search( r'a bc "def ghi" jk ').group()
    'def ghi'
    
    Note: this is NOT a good ptn for string, 'cos it can't handle " \" "
    
  '''
#

#. re: basic pattern ( id, str, num, sym )

#######################################################
# Identifier
#
# Identifier in OpenSCAD: two cases: starts with $ or not
# For OpenSCAD, these are valid identifiers: $123, 123ab_
# These are not : 123, 123$a, abc$def
#######################################################
RE_OPENSCAD_ID0 =  _nc_or( # Not capture 'or' (already captured below)
            _af( r'\W', r'\$\w+' ) ## That starts with $ 
                  ## $ and followed by word ( =[A-Za-z_0-9] )
                  ## after non-word to avoid 4$a, b$b
             , r'[0-9]*[A-Za-z_]+\w*\b'  ## That not starts with $  
           )
RE_OPENSCAD_ID = _nfb(  RE_OPENSCAD_ID0, r'\$' ) # not followed by $ 
RE_ID = RE_OPENSCAD_ID
RE_ID_C= _c(RE_ID)
   
def test_RE_ID():
  r'''     
     >>> p = re.compile( RE_ID)
     >>> f = lambda s: ( p.search(s) and 
     ...                 p.search(s).group()) or None  
     >>> f2 = lambda s: ( p.search(s) and 
     ...                 ( p.search(s).group(), p.search(s).groups() )
     ...               ) or None  
          
     --------------------------------
     >>> f( r' b=3; d_=4; 23c=5; $4=6;' ) 
     'b'
     >>> f( r' b$=3; d_=4; 23c=5; $4=6;' ) 
     'd_'
     >>> f( r' 23c=5; $4=6;' ) 
     '23c'
     >>> f( r' 23_=5; $4=6;' ) 
     '23_'
     >>> f( r' _23_=5; $4=6;' ) 
     '_23_'
     >>> f( r' $4=6;' ) 
     '$4'
     >>> f( r' $__=6;' ) 
     '$__'
     >>> f( r' 4$4=6;' ) 
     >>> f( r' ab$4=6;' ) 
     >>> f( r' 4b$4=6;' ) 
  '''           
#######################################################
# String 
#######################################################
RE_STR= r'(?s)("(?:\\"|.)*?")' # (?s): Dot matches newline characters
                               # (?: ): non capturing group. We capture
                               #         "xxx", but not xxx 
                               # \" needs to be considered 

## Openscad str: only "...", but no '...'. Also, "...
##  ..." is allowed (multi-line). So we don't need (?s):
RE_OPENSCAD_STR = '"'+ _nc('\\\\"|.')+'*?"'     
RE_STR = RE_OPENSCAD_STR
  
def test_RE_STR():
  r'''       
     >>> p = re.compile( RE_STR)
     >>> f = lambda s: ( p.search(s) and 
     ...                 p.search(s).group()) or None  
     
     --------------------------------
     >>> f( 'blah blah "abc" ...' ) 
     '"abc"'
     >>> f( r'blah blah "ab\"c" ...' ) 
     '"ab\\"c"'
  '''   

#######################################################
# Number
#######################################################
RE_NUM= r'-?\d*\.?\d+'
RE_NUM_C = _c(RE_NUM)  # non-capturing number
RE_NUM_NC= _nc(RE_NUM)  # non-capturing number
RE_NUM_LIST= ( r'\['+ _0ms() + RE_NUM_NC 
                   + _nc_0m(  _0ms()+','+_0ms() + RE_NUM_NC )
                   + _0ms() 
             + r'\]')
#######################################################
# Symbol
#######################################################
RE_SYM= r'[\{\},\<\>;\#\(\)\[\]=\.\*\+\|\$-\?/&@"\']'             

#. re: complex pattern: range, pt

## Matching a point = [x,y,z]
##
## RE_PT_2D or _3D only match [1,2,w]  (num or id) but not 
##  function call: [1,2, g(i)]
RE_PT_ITEM = _0ms()+_nc_or( RE_NUM, RE_ID)+ _0ms()
RE_PT2D = '\[' + RE_PT_ITEM + ',' + RE_PT_ITEM +'\]'
RE_PT3D = '\[' + RE_PT_ITEM + ',' + RE_PT_ITEM +',' + RE_PT_ITEM+ '\]'
 
def test_re_pt():
  '''
    
    >>> s='pt=[ w,-3,4.7 ]; '
    >>> m=re.compile( RE_PT3D ).search( s )
    >>> m.group()
    '[ w,-3,4.7 ]'
    
    >>> tk= list(tokenize(s, ( ('pt', RE_PT3D),)))[0]
    >>> tk
    Token(typ='pt', rng=(3, 15), rel_rng=(0, 3, 0, 15), txt='[ w,-3,4.7 ]')
  
  '''
 
#######################################################
# Range
#######################################################
RE_RANGE= ( r'\['+ _0ms() + RE_NUM 
                 + _nc_rep(  
                    _0ms()+':'+ _0ms() + _nc( RE_NUM)
                    , counts='1,2' )
                 + _0ms() 
          + r'\]') 

def test_RE_RANGE():
  r'''
    >>> p = re.compile( RE_RANGE )
    >>> f = lambda s: ( p.search(s) and 
    ...                 ( p.search(s).group(), p.search(s).groups() )
    ...               ) or None  

    --------------------------------
    >>> f(' blah [3: 4.5 ] blah ')       
    ('[3: 4.5 ]', ())
    
    >>> f(' blah [3:4.5:9] blah ')       
    ('[3:4.5:9]', ())
    
    >>> f(' blah [ -10:-5] blah ')       
    ('[ -10:-5]', ())
    
    >>> f(' blah [6:-2: -4] blah ')       
    ('[6:-2: -4]', ())

  '''
SYM_MUL = _naf_nfb( '/','\*','/' )
SYM_DIV = _naf_nfb( '\*','/','\*' )    
  
#. Token  

#######################################################
# A non-capturing "or" to pick up any simple pattern 
# ( str, num, sym, id). This is needed for scanner in
# the SKIP token to handle anything unwanted. 
#######################################################
RE_SIMPLE = _nc_or( RE_ID, RE_STR, RE_NUM, RE_SYM )
RE_SKIP = _nc_or( r'[ \t\n]',RE_ID, RE_STR, RE_NUM, RE_SYM )  

def ntToken(tkname, typ, rng, rel_rng, txt ):
  r'''
    Return a namedtuple, usually as a result of scanning,
    containing 4 attributes: 
    
      .typ      type of token
      .rng      (i,j) as the absolute range of token
      .rel_rng  (r1,c1,r2,c2) : the relative range. 
                r= line #, c = idx in line 
      .txt      the token content captured
    
    tkname must be a valid identifier.     
    
    >>> ntk = ntToken('blk','mod'
    ...  , (4,24),(0,4, 3, 5)
    ...  , 'this\n is a\ntest') # doctest: +NORMALIZE_WHITESPACE
    >>> ntk 
    blk(typ='mod', rng=(4, 24), rel_rng=(0, 4, 3, 5), txt='this\n is a\ntest')
    >>> ntk.typ
    'mod'
    >>> ntk.rng
    (4, 24)
    
    >>> str(ntk).split('(')[0]
    'blk'
        
  '''
  return collections.namedtuple( tkname,
          ['typ','rng','rel_rng','txt'])( typ, rng, rel_rng, txt )
 
def tokenize (s, rules, isAutoSkip=True): 
    r'''   
     
    Return a generator that spits out ntTokens (which is a 
    namedtuple), each is
    
      Token( typ, rng, rel_rng, txt ) 
    
    rule= (
            (token_name, token_re)
          , (token_name, token_re)      
          , (token_name, token_re)      
          , ('SKIP', RE_SKIP)      
          )
     
    NOTE: ALL elements in s must be handled, unlike: 
          re.compile(ptn).match(s):   match the begin of s
          re.compile(ptn).search(s):  match any part of s
    
          If any is not handle, error will occur. This is 
          avoided by adding a 'SKIP' item to rule. 
        
          If isAutoSkip, this SKIP will be added automatically
    --------------------------------------------------
    NOTE:
    
    tokenize(s
      , rules=((name1, re1), (name2, re2)...), isAutoSkip )    
    
      finds all tokens matching re1, re2 ... w/o specific order ---
      re2 can be matched before re1, or each can be matched multiple
      times. Note that every single letter/char/symbol (including 
      space) must be accounted for. (But, isAutoSkip=True can handle that)
    
    findTopComplexes( text, name
      , rules = (rule1, rule2, ...))
      
      rules example: ( RE_ID, ('\(','\)'), ('\{','\}') ) )
      finds all top level complexes matching all rules combined=
      rule1+rule2+...

    -----------------------------------------------------
             01234567890123456789012345678901234567890123456789
    >>> s = 'func(s="test")+5;'    
    
    >>> rules=( ('ASSIGN', RE_ID+'\='+RE_STR)
    ...       , ('SKIP', RE_SKIP) 
    ...       )
    
    >>> tks = tokenize( s, rules )
    
    >> tks # doctest: +ELLIPSIS 
    <generator object tokenizer at 0x...>
    
    >>> tks = list(tks) # doctest: +NORMALIZE_WHITESPACE
    >>> tks 
    [Token(typ='ASSIGN', rng=(5, 13), rel_rng=(0, 5, 0, 13), txt='s="test"')]
        
    >>> checkTokenRng( s, tks[0] )
    True
    
    -----------------------------------------------------
    Rules can skip the "SKIP" 'cos default isAutoSkip=True: 
    
    >>> rules2=[ ('ASSIGN', RE_ID+'\='+RE_STR) ]

    >>> tks=list(tokenize( s, rules2 )) # doctest: +NORMALIZE_WHITESPACE
    >>> tks
    [Token(typ='ASSIGN', rng=(5, 13), rel_rng=(0, 5, 0, 13), txt='s="test"')]
    
    >>> checkTokenRng( s, tks[0] )
    True
            
    -----------------------------------------------------
    >>> rules3=[ ('FUNC', RE_ID+ r'\(.*?\)')]
    >>> tks=list(tokenize( s, rules3 )) # doctest: +NORMALIZE_WHITESPACE
    >>> tks
    [Token(typ='FUNC', rng=(0, 14), rel_rng=(0, 0, 0, 14), txt='func(s="test")')]
    
    >>> checkTokenRng( s, tks[0] )
    True
        
    -----------------------------------------------------
    Note: the above rules3 is not a good one for finding function,
    'cos it fails to handle the following 2 conditions:
    
    >>> s2 = 'func( g(i) );'    
    
    The following is wrong:
    
    >>> list(tokenize( s2, rules3 )) # doctest: +NORMALIZE_WHITESPACE
    [Token(typ='FUNC', rng=(0, 10), 
      rel_rng=(0, 0, 0, 10), txt='func( g(i)')]
       
    >>> s3 = 'func( a="g(i)" );'    
    
    The following is wrong:
    
    >>> list(tokenize( s3, rules3 )) # doctest: +NORMALIZE_WHITESPACE
    [Token(typ='FUNC', rng=(0, 13), rel_rng=(0, 0, 0, 13), txt='func( a="g(i)')]       
       
    This has to be handled by find1stBlk() that uses recursion.
    
    -----------------------------------------------------
    String containing wanted tokens in multiline. 
    Note the rel_rng of the returned token.     
    
                  01234567890
    >>> s_ml = """var a=  4;
    ... b  =3.8; 
    ... c=-0.4; """
    
    >>> rules_set_num = [ 
    ...    ('SetNum', RE_ID+ _0ms()+'\='+_0ms() +RE_NUM ) ]
    
    >>> tks= list( tokenize(s_ml, rules_set_num ) ) 
    >>> tks  # doctest: +NORMALIZE_WHITESPACE
    [Token(typ='SetNum', rng=(4, 9), rel_rng=(0, 4, 0, 9), txt='a=  4'),
     Token(typ='SetNum', rng=(11, 18), rel_rng=(1, 0, 1, 7), txt='b  =3.8'),
     Token(typ='SetNum', rng=(21, 27), rel_rng=(2, 0, 2, 6), txt='c=-0.4')]    
    
    >>> [ checkTokenRng( s_ml, tk) for tk in tks] 
    [True, True, True]
       
    -----------------------------------------------------
    String containing wanted tokens that are in multiline. 
    
                  01234567890
    >>> s_ml2 = """var a=  
    ... 4;
    ... b  
    ... =3.8;"""
    
    >>> tks= list( tokenize(s_ml2, rules_set_num ) ) 
    >>> tks # doctest: +NORMALIZE_WHITESPACE
    [Token(typ='SetNum', rng=(4, 10), rel_rng=(0, 4, 1, 1), txt='a=  \n4'), Token(typ='SetNum', rng=(12, 20), rel_rng=(2, 0, 3, 4), txt='b  \n=3.8')] 
    
    >>> [ checkTokenRng( s_ml2, tk) for tk in tks] 
    [True, True]
     
    '''  
    if not 'SKIP' in [ x[0] for x in rules]:
        rules = tuple(rules) + (('SKIP',RE_SKIP),)
            
    keywords = {} #'//='} #, 'THEN', 'ENDIF', 'FOR', 'NEXT', 'GOSUB', 'RETURN'}

    tok_regex = '|'.join('(?P<%s>%s)' % pair for pair in rules)
    get_token = re.compile(tok_regex).match
    iline = 1
    pos = line_start = 0
    mo = get_token(s)
    while mo is not None:
        typ = mo.lastgroup
        if typ == 'NEWLINE':
            line_start = pos
            iline += 1
        elif typ != 'SKIP':
            txt = mo.group(typ)
            if typ == 'ID' and txt in keywords:
                typ = txt 
                
            abs_cBeg = mo.start()-line_start
            abs_cEnd = abs_cBeg + len( txt )
            
            abs_iline = iline
            lines = s[:abs_cBeg].split('\n')
            nline = len(lines) 
            c= len(lines[-1])
            txts = txt.split('\n')   
            ilastline = nline+len(txts)-1
            cEnd = len(txts)==1 and ( c+len(txt) ) or len(txts[-1])
            rel_rng= ( nline-1, c, ilastline-1, cEnd)
            
            tk= ntToken('Token'
              , typ
              , ( abs_cBeg, abs_cEnd )  # absolute range
              , rel_rng  
              , txt                     # the text matched
              ) 
              
            yield tk  
            
        pos = mo.end()
        mo = get_token(s, pos) 
        
    if pos != len(s):
        print('Something wrong in tokenize(). pos=%s, type=%s'%(
              pos, type(pos)))
        raise RuntimeError(
              'Unexpected character "%r" on line %d:\n"%s"' %(
              s[pos], iline, s[iline]))

def get_rel_rng(text, rng):
  r'''
    Given rng=(i,j), return rel_rng = (ilinebeg, cbeg, ilineend, cend)
      
                      012345678901234567890
    >>> get_rel_rng( 'Use get_rel_rng func', (4,15) )
    (0, 4, 0, 15)
    
    >>> s = """Use 
    ... the get_rel_rng 
    ... func nicely"""
    >>> rng = (9, 26)
    >>> s[ rng[0]:rng[1] ]
    'get_rel_rng \nfunc'
    >>> get_rel_rng( s, rng  )
    (1, 4, 2, 4)
      
    
  '''
  ibeg, iend = rng
  
  lines = text.split('\n')
  if len(lines)==1: return ( 0, ibeg, 0, iend )
  
  partialLines = text[:ibeg].split('\n')
  nLbeg = len( partialLines )-1
  cbeg = len( partialLines[-1] )
  
  partialLines2 = text[:iend].split('\n')
  nLend = len( partialLines2 )-1
  cend = len( partialLines2[-1] ) 
     
  return ( nLbeg, cbeg, nLend, cend )          
   
def checkTokenRng( text, ntTk, prnFail=False):
  r'''
    Chk if the ranges of a ntToken ( a namedtuple of
     (typ,rng,rel_rng,txt)) are correct
    
    Return True|False. If prnFail, also print fail msg when fails.
    
            01234567890
    >>> s= 'abc def ghi'
    >>> checkTokenRng( s, ntToken('tk','tk',(4,7),(0,4,0,7),'def'), True)
    True
    
    >>> s2= """abcde 
    ... fgh
    ... ij klmno""" 
    >>> tk = tokenize( s2, [( 'Data', 'fgh(\\n)ij')] )
    >>> tk = list(tk)[0]
    >>> tk 
    Token(typ='Data', rng=(7, 13), rel_rng=(1, 0, 2, 2), txt='fgh\nij')
         
    >>> s2[ tk.rng[0]:tk.rng[1] ]
    'fgh\nij'
        
    >>> checkTokenRng( s2, tk, True)
    True
  
    
  '''
  ntTkMustHas= ('typ', 'rng', 'rel_rng', 'txt')
  ntTkHas = dir(ntTk)
  isValid = all( [ (x in ntTkHas) for x in ntTkMustHas] )
  
  if not isValid:
    if prnFail:
      failmsg=( '\n###########################'
            + '\ntokenRngChk() failed=> ntTk(=%s) should have been'%str(ntTk)
            + '\na namedtuple containing following customized attributes:'
            #, '\n %s'%((ntkMustHas))
            )
      print(failmsg)
    return False    
  
  rng = ntTk.rng
  txt = text[ rng[0]: rng[1] ] 
  abs_rng_chk = txt== ntTk.txt
             
  rbeg,cbeg, rend,cend= ntTk.rel_rng
  lines =  text.split('\n')
  lines = [ line+ (r<len(lines)-1 and '\n' or '') 
            for r,line in enumerate(lines) ]
  
  def get_valid_line( iL, line ):
    #print('\n> get_valid_line(): \niL=%s, line[%s]="%s"'%(iL,iL,line))      

    rtn = { 1: ""
          , iL> rbeg and iL< rend: line
          , iL== rbeg: line[ cbeg: ]
          , iL== rend: line[ :cend ]
          , rbeg==rend==iL: line[ cbeg:cend ]    
          }[1]
    #print( 'rtn = "%s"'%rtn)
    #print('iL=%s, rtn="%s"'%(iL,rtn))      
    return rtn 
           
  txt = ''.join( [ get_valid_line(i,line) 
                  for i,line in enumerate(lines) ] )
                  
  rel_rng_chk = txt == ntTk.txt
  
  ispass = abs_rng_chk and rel_rng_chk
  
  if prnFail and not ispass:
                   
    failmsg=( '\n###########################'
            + '\ntokenRngChk() failed.'
            + '\n> token= %s:'%str(ntTk)
            + '\n> text ="%s"'%text
            + (not abs_rng_chk and 
               '\n> Failed rng: shouldn\'t be "%s"'%(txt) 
               or "" )
            + (not rel_rng_chk and 
             '\n> Failed rel_rng: shouldn\'t be "%s"'%(txt) 
             or "" )
            ) 
    print( failmsg )
  
  return ispass

''' Recursive matching [ ]

http://www.regular-expressions.info/recurse.html
http://rachbelaid.com/recursive-regular-experession/
http://www.rexegg.com/regex-recursion.html

Solution:
http://stackoverflow.com/questions/1099178/matching-nested-structures-with-regular-expressions-in-python

''' 

def re_rng( text, nt, d=None, moveto=None ):
    ''' re-arrange the rng and rel_rng of nt (ntToken) to 
      reflect its correct position in text. 
    
      The need arises when a nt is obtained with partial text,
      text[i:], from which the obtained nt might show rng=(0, n)
      where n is the length of nt.txt. This re_rng will correct it 
      to rng=(0+d, n+d) (in this case, d=i). The rel_rng, which 
      depends on # of linebreaks will also be adjusted using 
      get_rel_rng().
      
      d means difference and will be added to rng.
      
      moveto is an optional absolute index. If moveto is an int, 
      then the new rng will be: (moveto, moveto+n)

    First we found a ntToken from s:
    
             012345678901234 
    >>> s = 'func( g(i) );'    
    
    >>> blk= find1stBlk( s, name='()', ends=('\(', '\)') )
    >>> blk
    Blk(typ='()', rng=(4, 12), rel_rng=(0, 4, 0, 12), txt='( g(i) )')
    >>> checkTokenRng( s, blk )
    True
    
    Then we add text prior to s, then re_rng blk to its new pos :
    
    >>> added_text = '(Added text) '
    >>> s2 = added_text + 'func( g(i) );'
    >>> s2 
    '(Added text) func( g(i) );'
    
     0123456789012345678901234567
     
    >>> i = len(added_text)
    >>> blk2 = re_rng( s2, blk, i )
    >>> blk2
    Blk(typ='()', rng=(17, 25), rel_rng=(0, 17, 0, 25), txt='( g(i) )')
        
    >>> checkTokenRng( s2, blk2 )
    True
    
    ''' 
    if type(moveto)==int:
      rng = moveto, nt.rng[1]+moveto
    else:
      rng = nt.rng[0]+d, nt.rng[1]+d
    return ntToken( tkname= str(nt).split('(')[0]
                 , typ= nt.typ
                 , rng= rng
                 , rel_rng= get_rel_rng( text, rng )
                 , txt = nt.txt
                 )  
  
def token_at_i(nt_tokens, i):
  if nt_tokens:
    for nt in list( nt_tokens ):
      #if nt.rng[0]<=i and i < nt.rng[1]: return nt
      if isInTokenRng( nt, i ): return nt
      
def tokenize_num( s ):
  g_nt = tokenize( s, ( ('NUM', RE_NUM), ))
  return g_nt and list(g_nt) or None

def tokenize_str( s ):
  g_nt = tokenize( s, ( ('STR', RE_STR), ))
  return g_nt and list(g_nt) or None

def tokenize_id( s ):
  g_nt = tokenize( s, ( ('ID', RE_ID), ))
  return g_nt and list(g_nt) or None
  
def tokenize_pt( s, dim=3):
  '''
    >>> s=' pts= [[2,3,d], [w,h,k], [0,0,1] ]'
    >>> ntPts= tokenize_pt( s )
    >>> ntPts    # doctest: +NORMALIZE_WHITESPACE
    [Token(typ='PT', rng=(7, 14), rel_rng=(0, 7, 0, 14), txt='[2,3,d]'), 
     Token(typ='PT', rng=(16, 23), rel_rng=(0, 16, 0, 23), txt='[w,h,k]'),  
     Token(typ='PT', rng=(25, 32), rel_rng=(0, 25, 0, 32), txt='[0,0,1]')]
    
    >>> [ checkTokenRng(s, x) for x in ntPts ]
    [True, True, True]
        
  '''
  g_nt = tokenize( s, ( ('PT', dim==3 and RE_PT3D or RE_PT2D ),) )
  return g_nt and list(g_nt) or None
  
## keep this line here to ensure sectioning
    
#. Scanning blocks

"""
python re.Scanner examples:

https://docs.python.org/3.2/library/re.html#writing-a-tokenizer
http://www.programcreek.com/python/example/53972/re.Scanner
http://stackoverflow.com/questions/691148/pythonic-way-to-implement-a-tokenizer?lq=1
http://stackoverflow.com/questions/19280898/re-scanner-only-searching-start-of-string
"""

                     
def find1stBlk( text, name='{}', ends=("\{","\}")

              , _i=0
              , _iblkbegs=[]
              , _pbeg=None
              , _pend=None
              , _ntblk=None):
    r''' 
    Find the 1st-matched TOP level of a block defined by ends, where ends 
    = (bbeg, bend) in that both are valid string for re pattern. 
    Return a ntToken. 
    
             012345678901234 
    >>> s = 'func( g(i) );'    
    
    >>> blk= find1stBlk( s, name='()', ends=('\(', '\)') )
    >>> blk
    Blk(typ='()', rng=(4, 12), rel_rng=(0, 4, 0, 12), txt='( g(i) )')
    >>> checkTokenRng( s, blk )
    True
    
    >>> blk= find1stBlk( s, name='fname', ends=(RE_ID+'\(', '\)') )
    >>> blk
    Blk(typ='fname', rng=(0, 12), rel_rng=(0, 0, 0, 12), txt='func( g(i) )')
    >>> checkTokenRng( s, blk )
    True
    
              012345678901234
    >>> s2 = 'func(")");'        
    >>> blk= find1stBlk( s2, name='fname', ends=(RE_ID+'\(', '\)') )
    >>> blk
    Blk(typ='fname', rng=(0, 9), rel_rng=(0, 0, 0, 9), txt='func(")")')
    >>> checkTokenRng( s2, blk )
    True
    
              012345678901234567
    >>> s3 = 'a="("; func(")");'        
    >>> blk= find1stBlk( s3, name='fname', ends=(RE_ID+'\(', '\)') )
    >>> blk
    Blk(typ='fname', rng=(7, 16), rel_rng=(0, 7, 0, 16), txt='func(")")')
    >>> checkTokenRng( s3, blk )
    True
         
                012345678901234 
    >>> s4 = """func( 
    ... g(i) 
    ... );"""
    
    >>> ntBlk= find1stBlk( s4, name='()', ends=('\(', '\)') ) 
    >>> ntBlk
    Blk(typ='()', rng=(4, 14), rel_rng=(0, 4, 2, 1), txt='( \ng(i) \n)')
    >>> checkTokenRng( s4, ntBlk )
    True
    
    >>> s5 = "abcdefghijkl"
    >>> blk= find1stBlk( s5 )   # found nothing
    >>> blk
    
    >>> checkTokenRng( s5, blk )
    False
    
    '''
    #    print( 'ends = "%s"'%str(ends) )             
    #    return    

    if _i>=len(text) or _ntblk:
      #print('Leaving find1stBlk, _ntblk= ', _ntblk)
      return _ntblk    
    else:
    
      if _i==0: 
      
        _iblkbegs=[] ## Required to re-set it to []. Weird. It's 
                     ## supposed to be local, shouldn't be influenced
                     ## but previous run. But, it seems to be. 
        #print( 'ends = "%s"'%str(ends) )             
        _pbeg = re.compile( ends[0] )
        _pend = re.compile( ends[1] )                              
      
      ptext = text[_i:]
      
      mbeg = _pbeg.match(ptext)
       
      if mbeg:  
       
        _iblkbegs.append(_i)
        #print('   bbeg found, i=%s, _iblkbegs=%s'%(_i, str(_iblkbegs)))
        _i=_i+ len(mbeg.group()) 
           
      else:
      
        mend =_pend.match(ptext)
        
        if mend:
          #print('   bbend found, i=%s, _iblkbegs=%s'%(_i, str(_iblkbegs)))
          L = len(mend.group())
          
          if len(_iblkbegs)==1: #_iblkbegs!=None:

            rng= (_iblkbegs[0], _i+L)
            _ntblk = ntToken( 'Blk', typ=name
                            , rng = rng
                            , rel_rng = get_rel_rng( text, rng )
                            , txt = text[_iblkbegs[0]:_i+1]
                            )             
          elif _iblkbegs:
            _iblkbegs.pop()
                                     
          _i=_i+ L #len(mend.group()) 
 
        else:
          m= re.compile(RE_STR).match( text[_i:] )
          if m: _i= _i+len(m.group())
          else: _i= _i+1 
        
      ## NOTE: arg *ends* are used to generate re pattern at _i=0
      ##      (_pbeg and _pend). After then they are not needed.  
      return find1stBlk( text, name=name
             , _i=_i, _pbeg=_pbeg, _pend=_pend
             ,_iblkbegs=_iblkbegs, _ntblk=_ntblk
             )    

def findAllBlks( text, name='{}', ends=("\{","\}")

              , _i=0
              , _iblkbegs=[]
              , _pbeg=None
              , _pend=None
              , _ntblks=[]):
    r''' 
    Find all blks, including nested oncs, defined by ends, where ends 
    = (bbeg, bend) in that both are valid string for re pattern. 
    Return a list of ntTokens. In case of nested blks, the ntToken.rng
    will overlap:
    
      [ (... rng(4, 12)....)
      , (....rng(7, 10) ...)
      ]
      
             012345678901234 
    >>> s = 'func( g(i) );'    
    
    >>> blks= findAllBlks( s, name='()', ends=('\(', '\)') )
    >>> blks    # doctest: +NORMALIZE_WHITESPACE
    [Blk(typ='()', rng=(4, 12), rel_rng=(0, 4, 0, 12), txt='( g(i) )'), 
     Blk(typ='()', rng=(7, 10), rel_rng=(0, 7, 0, 10), txt='(i)')]   
    
    >>> [ checkTokenRng(s, blk) for blk in blks ]
    [True, True]
      
             012345678901234567890123456 
    >>> s2= 'pts= [ [2,3,4],[5,6,7] ]'
    >>> blks= findAllBlks( s2, name='[]', ends=('\[', '\]') )
    >>> blks    # doctest: +NORMALIZE_WHITESPACE
    [Blk(typ='[]', rng=(5, 24), rel_rng=(0, 5, 0, 24), txt='[ [2,3,4],[5,6,7] ]'), 
    Blk(typ='[]', rng=(7, 14), rel_rng=(0, 7, 0, 14), txt='[2,3,4]'), 
    Blk(typ='[]', rng=(15, 22), rel_rng=(0, 15, 0, 22), txt='[5,6,7]')]
        
    >>> [ checkTokenRng(s2, blk) for blk in blks ]
    [True, True, True]
    
    '''
    #    print( 'ends = "%s"'%str(ends) )             
    #    return    

    if _i>=len(text) :
      #print('Leaving find1stBlk, _ntblk= ', _ntblk)
      return _ntblks.sort() or _ntblks    
    else:
    
      if _i==0: 
        _ntblks = []
        _iblkbegs=[] ## Required to re-set it to []. Weird. It's 
                     ## supposed to be local, shouldn't be influenced
                     ## but previous run. But, it seems to be. 
        #print( 'ends = "%s"'%str(ends) )
                     
        _pbeg = re.compile( ends[0] )
        _pend = re.compile( ends[1] )                              
      
      ptext = text[_i:]
      
      mbeg = _pbeg.match(ptext)
       
      if mbeg:  
       
        _iblkbegs.append(_i)
        #print('   bbeg found, i=%s, _iblkbegs=%s'%(_i, str(_iblkbegs)))
        _i=_i+ len(mbeg.group()) 
           
      else:
      
        mend =_pend.match(ptext)
        
        if mend:
          #print('   bbend found, i=%s, _iblkbegs=%s'%(_i, str(_iblkbegs)))
        
          if _iblkbegs: #_iblkbegs!=None:
          
            L = len(mend.group())
            ibeg= _iblkbegs.pop()
            rng= ( ibeg, _i+L)
            _ntblk = ntToken( 'Blk', typ=name
                            , rng = rng
                            , rel_rng = get_rel_rng( text, rng )
                            , txt = text[ibeg:_i+L]
                            )             
            _ntblks.append( _ntblk )
                                     
            _i=_i+ L 
 
        else:
          m= re.compile(RE_STR).match( text[_i:] )
          L = m and len(m.group()) or 1
          _i= _i+L 
        
      ## NOTE: arg *ends* are used to generate re pattern at _i=0
      ##      (_pbeg and _pend). After then they are not needed.  
      return findAllBlks( text, name=name
             , _i=_i, _pbeg=_pbeg, _pend=_pend
             ,_iblkbegs=_iblkbegs, _ntblks=_ntblks
             )    

def findTopBlks( text, name='{}', ends=("\{","\}")
            , _i=0, _rtn=[] ):
  '''
    Return a list containing all ntToken as top-level blocks
    defined by ends. 
    
    >>> s = """a=[2,3,4]; b=[5,6];"""
    >>> blks= findTopBlks(s, '[]', ends=('\[','\]'))
    >>> blks        # doctest: +NORMALIZE_WHITESPACE
    [Blk(typ='[]', rng=(2, 9), rel_rng=(0, 2, 0, 9), txt='[2,3,4]'), 
     Blk(typ='[]', rng=(13, 18), rel_rng=(0, 13, 0, 18), txt='[5,6]')] 
    >>> [ checkTokenRng(s, x) for x in blks ]
    [True, True]
  
    >>> s2 = """a=[2,[3,4]]; b=[[5,6],"]"];"""
    >>> blks= findTopBlks(s2, '[]', ends=('\[','\]'))
    >>> blks        # doctest: +NORMALIZE_WHITESPACE
    [Blk(typ='[]', rng=(2, 11), rel_rng=(0, 2, 0, 11), txt='[2,[3,4]]'), 
     Blk(typ='[]', rng=(15, 26), rel_rng=(0, 15, 0, 26), txt='[[5,6],"]"]')]
    >>> [ checkTokenRng(s2, x) for x in blks ]
    [True, True]
    
  '''
  if _i>=len(text):
    #print('Leaving find1stBlk, _ntblk= ', _ntblk)
    return _rtn
        
  else:
    if _i==0: _rtn=[]
    
    nt = find1stBlk( text[_i:], name=name, ends=ends )
    if nt:
      nt = re_rng( text, nt, _i )            
      _rtn.append( nt )
      _i =nt.rng[1] 
    else:
      _i = _i +1
         
    return findTopBlks( text, name=name, ends=ends, _i=_i, _rtn=_rtn )              
  
  
def find1stComplex(text, name, rules

                   , _i=0 
                   , _nextRule=0
                   , _rtn=[]
                   ):
  r'''
  
    Find the 1st continuously connected patterns, return (blk, items)
    where items is a list of ntTokens corresponding to each rules,
    and blk is a ntToken of all items combined.

    rules = list of patterns, where each pattern is either
            a string or a tuple of (beg,end) for patterns of
            block beg and end, respectively.
            
            rules= ( ptn, (ptn_blkbeg, ptn_blkend) ...)
            
            
    Note: 1) White spaces between rules are ignored.   
          2) The blocks matched by (ptn_blkbeg, ptn_blkend) is 
             done by find1stBlk(), which will match only the top
             level block.
           
    --------------------------------------------------
             012345678901234567890123 
    >>> s = 'a=mod( g(i) )   { blah } ;'
    
    >>> blk, items = find1stComplex( s, 'test',
    ... ( ('\(','\)'), ('\{','\}') ) ) 
    
    >>> blk               # doctest: +NORMALIZE_WHITESPACE
    Blk(typ='test', rng=(5, 24), rel_rng=(0, 5, 0, 24), 
      txt='( g(i) )   { blah }')
    
    >>> items             # doctest: +NORMALIZE_WHITESPACE
    [Item(typ='\\(\\)', rng=(5, 13), rel_rng=(0, 5, 0, 13), txt='( g(i) )'), Item(typ='\\{\\}', rng=(16, 24), rel_rng=(0, 16, 0, 24), txt='{ blah }')]    
    >>> checkTokenRng( s, blk )
    True
    >>> checkTokenRng( s, items[0] )
    True
    >>> checkTokenRng( s, items[1] )
    True
    
    --------------------------------------------------
    We double the s by repeating itself. The result is the same, 'cos
    find1stComplex() only return the first one:
    
    >>> s = 'a=mod( g(i) )   { blah } ;a=mod( g(i) )   { blah } ;'
    
    >>> blk,items= find1stComplex( s, 'test',
    ... ( ('\(','\)'), ('\{','\}') ) ) 
    
    >>> blk               # doctest: +NORMALIZE_WHITESPACE
    Blk(typ='test', rng=(5, 24), rel_rng=(0, 5, 0, 24), 
      txt='( g(i) )   { blah }') 
         
    --------------------------------------------------
    
                012345678901234567890123 
    >>> s2 = """a=mod( g(i) )
    ... { blah
    ... } ;"""
    
    >>> blk,items = find1stComplex( s2, 'modcall',
    ... ( RE_ID,  ('\(','\)')  , ('\{','\}') ) )
    
    >>> blk
    Blk(typ='modcall', rng=(2, 22), rel_rng=(0, 2, 2, 1), txt='mod( g(i) )\n{ blah\n}')
    
    >>> items     # doctest: +NORMALIZE_WHITESPACE
    [Item(typ='MultiBlk_Item', rng=(2, 5), rel_rng=(0, 2, 0, 5), txt='mod'), 
     Item(typ='\\(\\)', rng=(5, 13), rel_rng=(0, 5, 0, 13), txt='( g(i) )'), 
     Item(typ='\\{\\}', rng=(14, 22), rel_rng=(1, 0, 2, 1), txt='{ blah\n}')]
        
    >> checkTokenRng( s2, blk )
    True
    
    >> [ checkTokenRng(s2, x) for x in items ]
    [True, True, True]
    
    --------------------------------------------------
    
              01234567890123456789012345678901234567890
    >>> s3 = 'mod(x){blah;} a=3 day(g(i)){ blah;}'
    >>> cpx= find1stComplex(s3, 'testing', ( RE_ID, ('\(','\)'), ('\{','\}') ) )
    >>> cpx      # doctest: +NORMALIZE_WHITESPACE
    (Blk(typ='testing', rng=(0, 13), rel_rng=(0, 0, 0, 13), txt='mod(x){blah;}'), 
    [Item(typ='MultiBlk_Item', rng=(0, 3), rel_rng=(0, 0, 0, 3), txt='mod'), 
     Item(typ='\\(\\)', rng=(3, 6), rel_rng=(0, 3, 0, 6), txt='(x)'), 
     Item(typ='\\{\\}', rng=(6, 13), rel_rng=(0, 6, 0, 13), txt='{blah;}')])
    
        
  '''
  # print( '>>> _i=%s, _nextRule=%s'%(_i,_nextRule)) 
  
  if _i>=len(text) or _nextRule>=len(rules) :
    #print('Leaving find1stBlk, _ntblk= ', _ntblk)
    if _rtn:
      rng= ( _rtn[0].rng[0], _rtn[-1].rng[1] )
      combined = ntToken( tkname='Blk'
                        , typ= name
                        , rng= rng
                        , rel_rng= get_rel_rng( text, rng )
                        , txt = text[ rng[0]:rng[1] ]
                        )
      _rtn= ( combined, _rtn )                    
                      
    return _rtn  
      
  else:
    if _i==0: _rtn = []  ### Somehow _rtn has to be reset at _i=0. Weird. 
    
    ntItem= None
      
    ## Match 1 or more white space to skip them:
    m= re.compile(_1ms()).match( text[_i:])
    if m:
      _i= _i+ len(m.group())
      
    else:
      #print( '>>> _i=%s, text[%s]="%s", rule="%s"'%(_i,_i,text[_i:],str(rules[_nextRule])))
      rule = rules[_nextRule]
      #print( 'type(rule)= %s, rule = "%s"'%(type(rule), rule))
      
      if type(rule)==str:
      
        m= re.compile(rule).match(text[_i:])
        if m:
          txt = m.group()
          rng = ( 0, len(txt) )
          ntItem = ntToken( tkname=txt
                       , typ='MultiBlk_Item'
                       , rng= rng
                       , rel_rng= get_rel_rng( text, rng )
                       , txt = txt
                       )                       
      else:
        blk = find1stBlk(text[_i:], name=''.join(rule), ends= rule )
              ## The name above is for individual blk. It can only
              ## be seen during a debug when print(...)
        
        ## find1stBlk(txt, ...) find block match anywhere inside txt, 
        ## doesn't have to be the beginning. But we need it to be 
        ## in the beginning, so discard it if not.       
        if (blk and blk.rng[0]==0 ):
          ntItem = blk
      
      if ntItem:      ## ntItem found !!
        
        rng = (_i, _i+len(ntItem.txt))
        ntItem = ntToken( tkname = 'Item'
                        , typ = ntItem.typ
                        , rng = rng 
                        , rel_rng = get_rel_rng(text, rng)
                        , txt = ntItem.txt
                        )
        _rtn.append( ntItem )

        _i= ntItem.rng[1] #_i + len(ntItem.txt)
        _nextRule = _nextRule + 1
      
      else:         ## We already make sure any ntItem.rng[0]=0, 
                    ## so only need to check if it exist. If not,
                    ## revert back to the beginning of _rtn list
        if _rtn: _i= _rtn[0].rng[0]+1
        else   : _i= _i+1
        _rtn= []
        _nextRule = 0
       
    #    print( '-- b4 return, ntItem=%s'%str(ntItem))                 
    #    print( '-- b4 return, next _i=%s, _nextRule=%s'%(_i,_nextRule))                 
    #    print( '-- b4 return, _rtn=%s'%str(_rtn))                 
    return find1stComplex(text
                   , name
                   , rules
                   , _i 
                   , _nextRule
                   , _rtn=_rtn
                   )                            
  
def findTopComplexes( text, name, rules
            , _i=0, _rtn=[] ):
  r'''
    Return a list containing all ntToken as top-level blocks
    defined by ends. 
    
    --------------------------------------------------
    NOTE:
    
    tokenize(s
      , rules=((name1, re1), (name2, re2)...), isAutoSkip )    
    
      finds all tokens matching re1, re2 ... w/o specific order ---
      re2 can be matched before re1, or each can be matched multiple
      times. Note that every single letter/char/symbol (including 
      space) must be accounted for. (But, isAutoSkip=True can handle that)
    
    findTopComplexes( text, name
      , rules = (rule1, rule2, ...))
      
      rules example: ( RE_ID, ('\(','\)'), ('\{','\}') ) )
      finds all top level complexes matching all rules combined=
      rule1+rule2+...
          
    --------------------------------------------------
             012345678901234567890123 
    >>> s = 'a=mod( g(i) )   { blah } ;'
    
    >>> blk, items = find1stComplex( s, 'test',
    ... ( ('\(','\)'), ('\{','\}') ) ) 
    
    >>> blk               # doctest: +NORMALIZE_WHITESPACE
    Blk(typ='test', rng=(5, 24), rel_rng=(0, 5, 0, 24), 
      txt='( g(i) )   { blah }')
      
    >>> items              # doctest: +NORMALIZE_WHITESPACE
    [Item(typ='\\(\\)', rng=(5, 13), rel_rng=(0, 5, 0, 13), txt='( g(i) )'), 
    Item(typ='\\{\\}', rng=(16, 24), rel_rng=(0, 16, 0, 24), txt='{ blah }')]      
  
    --------------------------------------------------
              01234567890123456789012345678901234567890
    >>> s2 = 'mod(x){blah;} a=3 day(g(i)){ blah;}'
    >>> cps = find1stComplex(s2, 'testing', ( RE_ID, ('\(','\)'), ('\{','\}') ) )
    >>> cps               # doctest: +NORMALIZE_WHITESPACE
    (Blk(typ='testing', rng=(0, 13), rel_rng=(0, 0, 0, 13), txt='mod(x){blah;}'), 
    [Item(typ='MultiBlk_Item', rng=(0, 3), rel_rng=(0, 0, 0, 3), txt='mod'), 
    Item(typ='\\(\\)', rng=(3, 6), rel_rng=(0, 3, 0, 6), txt='(x)'), 
    Item(typ='\\{\\}', rng=(6, 13), rel_rng=(0, 6, 0, 13), txt='{blah;}')]) 

        
    >>> blk1, blk2 = findTopComplexes( s2, 'test2',
    ... ( RE_ID, ('\(','\)'), ('\{','\}') ) ) 
    
    >>> blk1    # doctest: +NORMALIZE_WHITESPACE
    (Blk(typ='test2', rng=(0, 13), rel_rng=(0, 0, 0, 13), txt='mod(x){blah;}'), 
    [Item(typ='MultiBlk_Item', rng=(0, 3), rel_rng=(0, 0, 0, 3), txt='mod'), 
    Item(typ='\\(\\)', rng=(3, 6), rel_rng=(0, 3, 0, 6), txt='(x)'), 
    Item(typ='\\{\\}', rng=(6, 13), rel_rng=(0, 6, 0, 13), txt='{blah;}')])    
        
    >>> blk2    # doctest: +NORMALIZE_WHITESPACE
    (Blk(typ='test2', rng=(18, 35), rel_rng=(0, 18, 0, 35), txt='day(g(i)){ blah;}'), 
    [Item(typ='MultiBlk_Item', rng=(18, 21), rel_rng=(0, 18, 0, 21), txt='day'), 
    Item(typ='\\(\\)', rng=(21, 27), rel_rng=(0, 21, 0, 27), txt='(g(i))'), 
    Item(typ='\\{\\}', rng=(27, 35), rel_rng=(0, 27, 0, 35), txt='{ blah;}')])
    
    >>> checkTokenRng( s2, blk1[0] )
    True
    >>> [ checkTokenRng( s2, x ) for x in blk1[1] ]
    [True, True, True]

    >>> checkTokenRng( s2, blk2[0] )
    True
    >>> [ checkTokenRng( s2, x ) for x in blk2[1] ]
    [True, True, True]
             
    --------------------------------------------------
    >>> s2 = """a=[2,[3,4]]; b=[[5,6],"]"];"""
    >>> blks= findTopBlks(s2, '[]', ends=('\[','\]'))
    >>> blks        # doctest: +NORMALIZE_WHITESPACE
    [Blk(typ='[]', rng=(2, 11), rel_rng=(0, 2, 0, 11), txt='[2,[3,4]]'), 
     Blk(typ='[]', rng=(15, 26), rel_rng=(0, 15, 0, 26), txt='[[5,6],"]"]')]
    >>> [ checkTokenRng(s2, x) for x in blks ]
    [True, True]
    
  '''
  #print('>>> findTopComplexes, _i=%s'%(_i)) 
  if _i>=len(text):
    #print('Leaving find1stBlk, _ntblk= ', _ntblk)
    return _rtn
        
  else:
    if _i==0: _rtn=[]
    cpx = find1stComplex( text[_i:], name=name, rules=rules )
    
    if cpx:
      nt, items = cpx
      ## Need to re_rng for all elements:
      nt = re_rng(text, nt, _i)
      #_items = [ re_rng(x, text, moveto=x.rng[1]) 
      #          for j,x in enumerate( [nt]+items ) if j>0]
      items = [ re_rng( text, x, _i) for x in items ]
      
      _rtn.append( (nt,items) )
      _i = nt.rng[1]
    else:
      print('  failed to find complex, _i=%s'%_i)
      _i = _i +1
    #print( '<<< leaving, _i=%s'%(_i))     
    return findTopComplexes( text, name=name, rules=rules,
                             _i=_i, _rtn=_rtn )              
  
  
def find1stCurlyBlk(text):
  return find1stBlk(text, name='{}', ends=("\{","\}"))

def find1stSqBlk(text):
  return find1stBlk(text, name='[]', ends=("\[", "\]"))

def find1stRoundBlk(text):
  '''
           012345678901234567890123456789012 
    >>> s='a=3; move([(2,3),4])  { a=3; }; '
    >>> cblk = find1stRoundBlk( s)
    >>> cblk 
    Blk(typ='()', rng=(9, 20), rel_rng=(0, 9, 0, 20), txt='([(2,3),4])')
    
    >>> checkTokenRng( s, cblk, 1 )
    True
    '''
    
  return find1stBlk(text, name='()', ends=("\(", "\)"))
  
def find1stFuncCall(text):
  return find1stComplex( text, name='funcCall'
          , rules= ( RE_ID, ("\(", "\)") ) )
                       
def find1stModBlk(text):
  '''      
    >>> s='a=3; move([(2,3),4]){ cube(); }; '
    >>> modblk = find1stModBlk( s)
    >>> modblk 
    Blk(typ='modBlk', rng=(5, 31), rel_rng=(0, 5, 0, 31), txt='move([(2,3),4]){ cube(); }')
    >>> checkTokenRng( s, modblk )
    True
     
  '''
  
  rtn= find1stComplex(text, name='modBlk', 
          rules= ( RE_ID, ("\(", "\)") , ("\{", "\}") )
                        )
  if rtn: rtn= rtn[0]
  return rtn
 
def findAllLists( s ):
  return findAllBlks( s, name='[]', ends=('\[', '\]') )
  
def isInTokenRng(tk,i):
  return tk.rng[0]<=i and i<tk.rng[1]  
  
#. Is token at i ?

def ntNum_at_i( s, i ):
  '''
             012345678901234567890
    >>> s = 'a= 23.5 + abc* (-0.1)'
    >>> s[5], s[16]
    ('.', '-')
    >>> ntNum_at_i( s, 5)
    Token(typ='NUM', rng=(3, 7), rel_rng=(0, 3, 0, 7), txt='23.5')
    
    >>> ntNum_at_i( s, 16)
    Token(typ='NUM', rng=(16, 20), rel_rng=(0, 16, 0, 20), txt='-0.1')
    
    >>> ntNum_at_i( s, 15)  # this returns None
        
  '''
  return token_at_i( tokenize_num( s ), i )

def ntStr_at_i( s, i ):
  '''
             012345678901234567890
    >>> s = 'a= "abc" + "def"'
    >>> s[3], s[13]
    ('"', 'e')
    >>> ntStr_at_i( s, 3)
    Token(typ='STR', rng=(3, 8), rel_rng=(0, 3, 0, 8), txt='"abc"')
        
    >>> ntStr_at_i( s, 15)
    Token(typ='STR', rng=(11, 16), rel_rng=(0, 11, 0, 16), txt='"def"')
        
    >>> ntStr_at_i( s, 16)  # this returns None
        
  '''
  return token_at_i( tokenize_str( s ), i ) 
  
def ntId_at_i( s, i ):
  '''
             012345678901234567890
    >>> s = 'a= func(def)'
    >>> s[4], s[10]
    ('u', 'f')
    >>> ntId_at_i( s, 4)
    Token(typ='ID', rng=(3, 7), rel_rng=(0, 3, 0, 7), txt='func')
        
        
    >>> ntId_at_i( s, 10)
    Token(typ='ID', rng=(8, 11), rel_rng=(0, 8, 0, 11), txt='def')
        
        
    >>> ntId_at_i( s, 12)  # this returns None
        
  '''
  return token_at_i( tokenize_id( s ), i ) 

def ntPt_at_i( s, i, dim=3 ):
  '''
           012345678901234567890
    >>> s=' pts=[ [a,b,c], [2,3,w] ]; '
    >>> ntPt_at_i( s,9)  # doctest: +NORMALIZE_WHITESPACE
    [Token(typ='PT', rng=(7, 14), rel_rng=(0, 7, 0, 14), txt='[a,b,c]')]
        
    >>> ntPt_at_i( s,15)    # Return nothing
       
  '''   
  
  ntPts = tokenize_pt( s, dim=dim )
  rtn=None
  if ntPts:
    rtn= [ nt for nt in ntPts if isInTokenRng( nt, i ) ]
    if rtn: return rtn
    
  
def ntLists_at_i(s,i):
  '''
    
           012345678901234567890
    >>> s=' pts=[ [a,b,c], [2,3,w] ]; '
    >>> ntLists_at_i( s,9)  # doctest: +NORMALIZE_WHITESPACE
    [Blk(typ='[]', rng=(5, 25), rel_rng=(0, 5, 0, 25), txt='[ [a,b,c], [2,3,w] ]'), 
    Blk(typ='[]', rng=(7, 14), rel_rng=(0, 7, 0, 14), txt='[a,b,c]')]    
    
    >>> ntLists_at_i( s,15)
    [Blk(typ='[]', rng=(5, 25), rel_rng=(0, 5, 0, 25), txt='[ [a,b,c], [2,3,w] ]')]
        
  '''   
  
  ntlists = findAllLists( s )
  if ntlists:
    return [ nt for nt in ntlists if isInTokenRng( nt, i ) ]

 
#. Tests


def findPtBlk( text ):
  '''
  '''
  
def re_test():
  ''' Test the re and token using doctest
  ''' 
  def test_docstr(func): 
      print('>>> doctesting: '+ func.__name__ +'() ')
      doctest.run_docstring_examples(func, globals())                                                
  
  funcs = ( 
           'RE'
          , _or_items
          , test_re_or
          , _1m
          , _nc_1m
          , _rep
          , test_affb 
          , test_RE_ID
          , test_RE_STR
          , test_re_pt
          , test_RE_RANGE
          
          , 'Token' 
          
          , ntToken
          , tokenize          
          , get_rel_rng
          , re_rng
          , checkTokenRng
          , tokenize_pt
          
          , 'Token at i'
          
          , ntNum_at_i
          , ntStr_at_i
          , ntId_at_i
          , ntLists_at_i
          , ntPt_at_i
          
          , 'Scanning block'
          
          , find1stBlk
          , findAllBlks
          , findTopBlks
          , find1stComplex
          , findTopComplexes
          
          , find1stRoundBlk
          , find1stModBlk
          )
  def dumb():pass
  for f in funcs: 
    if type(f)== type(dumb):
      test_docstr( f ) 
    else:
      print('====== %s ======'%f)
      
if __name__=='__main__':
    re_test()
    #import sys
    #print sys.version
    