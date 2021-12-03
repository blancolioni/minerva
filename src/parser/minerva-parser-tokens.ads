private package Minerva.Parser.Tokens is

   type Token is
      (Tok_None, Tok_End_Of_File, Tok_Bad_Character,
       Tok_Identifier, Tok_Integer_Constant, Tok_Floating_Point_Constant,
       Tok_Character, Tok_String_Constant,

       Tok_And, Tok_Array, Tok_Begin, Tok_Body, Tok_Case, Tok_Constant,
       Tok_Declare, Tok_Div, Tok_Do, Tok_Else, Tok_Elsif, Tok_End, Tok_For,
       Tok_Function, Tok_Goto, Tok_If, Tok_In, Tok_Is, Tok_Loop, Tok_Mod,
       Tok_Not, Tok_Null, Tok_Of, Tok_Or, Tok_Out, Tok_Package, Tok_Procedure,
       Tok_Record, Tok_Return, Tok_Then, Tok_Type, Tok_Use,
       Tok_When, Tok_While, Tok_With, Tok_Xor,

       Tok_Colon, Tok_Semicolon, Tok_Comma,

       Tok_Equal, Tok_Not_Equal,
       Tok_Greater, Tok_Greater_Equal,
       Tok_Less, Tok_Less_Equal,

       Tok_Ampersand, Tok_Asterisk, Tok_Slash, Tok_Plus, Tok_Minus,

       Tok_Left_Paren, Tok_Right_Paren,

       Tok_Dot, Tok_Dot_Dot, Tok_Right_Arrow, Tok_Vertical_Bar,

       Tok_Assign, Tok_Left_Label, Tok_Right_Label, Tok_Box);

   subtype Tok_Reserved_Word is Token range Tok_And .. Tok_Xor;

end Minerva.Parser.Tokens;
