{

} 

let tchar = ['a'-'z' 'A'-'Z' '0'-'9']

rule request_line = parse 
    | ' ' { SP }
    | "\r\n" { CRLF }
    | tchar+ { METHOD }