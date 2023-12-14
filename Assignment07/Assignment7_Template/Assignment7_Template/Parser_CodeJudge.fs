module ImpParser

    open System
    open JParsec.TextParser
    //open FParsecLight.TextParser
    open Eval
    open Types
    
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"

    let pif         = pstring "if"
    let pthen       = pstring "then"
    let pelse       = pstring "else"
    let pendif      = pstring "endif"
    let pwhile      = pstring "while"
    let pdo         = pstring "do"
    let pendwhile   = pstring "endwhile"
    let pdeclare    = pstring "declare"
    
    let whitespaceChar = satisfy (Char.IsWhiteSpace) <?> "whitespace"

    let spaces = many whitespaceChar <?> "spaces"
    let spaces1 = many1 whitespaceChar <?> "space1"

    let pletter = satisfy Char.IsLetter <?> "letter"

    let palphanumeric = satisfy Char.IsLetterOrDigit <?> "alphanumeric"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2 = p1 .>> spaces .>> p2
    let (>*>.) p1 p2 = p1 .>>. spaces >>. p2

    let unop = (>*>.)
    let binop op p1 p2 = p1 .>*> op .>*>. p2

    let parenthesise c = pchar '(' >*>. c .>*> pchar ')'
    let curlies c = pchar '{' >*>. c .>*> pchar '}'

    let pid = 
         (pletter <|> pchar '_') .>>. many (palphanumeric <|> pchar '_') |>> 
         (fun (x, xs) -> List.fold (+) (string x) (List.map string xs)) <?> "identifier"

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()

    let NParse   = pint32 |>> N <?> "Int"
    let VParse   = pid |>> V <?> "Var"

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> Mul(N (-1), x)) <?> "Neg"
    let CTIParse = unop pCharToInt (parenthesise CharParse) |>> CharToInt <?> "charToInt"
    let PVParse  = unop pPointValue (parenthesise TermParse) |>> PV <?> "pointValue"
    let ParParse = parenthesise TermParse
    do aref := choice [NegParse; CTIParse; PVParse; ParParse; NParse; VParse]

    let CharLitParse   = pchar '\'' >>. anyChar .>> pchar '\'' |>> C <?> "Char literal"
    let CharValueParse = unop pCharValue (parenthesise TermParse) |>> CV <?> "charValue"
    let ToUpperParse   = unop pToUpper (parenthesise CharParse) |>> ToUpper <?> "ToUpper"
    let ToLowerParse   = unop pToLower (parenthesise CharParse) |>> ToLower <?> "toLower"
    let ITCParse       = unop pIntToChar (parenthesise TermParse) |>> IntToChar <?> "intToChar"
    do cref := choice [CharLitParse; CharValueParse; ToUpperParse; ToLowerParse; ITCParse]

    let AexpParse = TermParse 
    let CexpParse = CharParse

    let LParse, lref = createParserForwardedToRef<bExp>()
    let BParse, bref = createParserForwardedToRef<bExp>()

    let EqParse  = binop (pchar '=') AexpParse AexpParse |>> (fun p -> p ||> (.=.)) <?> "Eq"
    let NeqParse = binop (pstring "<>") AexpParse AexpParse |>> (fun p -> p ||> (.<>.)) <?> "Neq"
    let LtParse  = binop (pchar '<') AexpParse AexpParse |>> (fun p -> p ||> (.<.)) <?> "Lt"
    let LteParse = binop (pstring "<=") AexpParse AexpParse |>> (fun p -> p ||> (.<=.)) <?> "Lte"
    let GtParse  = binop (pchar '>') AexpParse AexpParse |>> (fun p -> p ||> (.>.)) <?> "Gt"
    let GteParse = binop (pstring ">=") AexpParse AexpParse |>> (fun p -> p ||> (.>=.)) <?> "Gte"
    let RelParse = choice [EqParse; NeqParse; LtParse; LteParse; GtParse; GteParse; BParse]

    let ConjParse = binop (pstring "/\\") RelParse LParse |>> (fun p -> p ||> (.&&.)) <?> "Conj"
    let DisjParse = binop (pstring "\\/") RelParse LParse |>> (fun p -> p ||> (.||.)) <?> "Disj"
    do lref := choice [ConjParse; DisjParse; RelParse]

    let BNegParse = unop (pchar '~') BParse |>> ((~~))
    let BTrueParse = pTrue |>> fun _ -> TT
    let BFalseParse = pFalse |>> fun _ -> FF
    let BParParse = parenthesise LParse
    do bref := choice [BNegParse; BTrueParse; BFalseParse; BParParse]

    let BexpParse = LParse

    let StmntParse, sref = createParserForwardedToRef<stmnt>()
    let SParse, seqref   = createParserForwardedToRef<stmnt>()

    let IT(b, c) = ITE(b, c, Skip)

    let SeqParse    = binop (pstring ";") StmntParse SParse |>> Seq <?> "sequence"
    do seqref := SeqParse <|> StmntParse

    let AssParse     = binop (pstring ":=") pid AexpParse |>> Ass <?> "assignment"
    let DeclareParse = pdeclare >>. whitespaceChar >*>. pid |>> Declare <?> "declare"
    let ITEParse     = pif >*>. parenthesise BexpParse .>*> 
                       pthen .>*>. curlies SParse .>*> 
                       pelse .>*>. curlies SParse |>> 
                       (fun ((b, c1), c2) -> ITE(b, c1, c2)) <?> "if-then-else"
    let ITParse      = pif >*>. parenthesise BexpParse .>*>
                       pthen .>*>. curlies SParse |>> IT <?> "if-then"
    let WhileParse   = pwhile >*>. 
                       parenthesise BexpParse .>*>
                       pdo .>*>.
                       curlies SParse |>> While <?> "while"

    do sref := choice [AssParse; DeclareParse; ITEParse; ITParse; WhileParse]

    let stmntParse = SParse
    
    let parseSquareProg (sqp : squareProg) : square =
        Map.map (fun _ -> run stmntParse >> getSuccess >> stmntToSquareFun) sqp
 
    type squareFun2 = coord -> StateMonad.Result<square option, StateMonad.Error>
    let parseBoardProg prog (sqs : Map<int, square>) : squareFun2 =
        prog |>
        run stmntParse |>
        getSuccess |>
        stmntToBoardFun <|
        sqs
    
    let mkBoard (bp : boardProg) =
        let sqs =  (Map.map (fun _ -> parseSquareProg) bp.squares)
        {
            center = bp.center;
            defaultSquare = Map.find bp.usedSquare sqs
            squares = parseBoardProg bp.prog sqs
        }
