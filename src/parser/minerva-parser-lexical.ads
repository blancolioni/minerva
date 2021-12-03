with GCS.Lexer;
with GCS.Styles;                           use GCS.Styles;

with Minerva.Parser.Tokens;                use Minerva.Parser.Tokens;

pragma Elaborate_All (GCS.Lexer);

private package Minerva.Parser.Lexical is
  new GCS.Lexer (Token              => Token,
                 Tok_None           => Tok_None,
                 Tok_End_Of_File    => Tok_End_Of_File,
                 Tok_Bad_Character  => Tok_Bad_Character,
                 Tok_Identifier     => Tok_Identifier,
                 Tok_String         => Tok_String_Constant,
                 Tok_Character      => Tok_Character,
                 Tok_Integer        => Tok_Integer_Constant,
                 Tok_Float          => Tok_Floating_Point_Constant,
                 First_Keyword      => Tok_And,
                 Keywords           =>
                    "and array begin body case constant declare div "
                 & "do else elsif end for function goto "
                 & "if in is loop mod not null of "
                 & "or out package procedure record return then type "
                 & "use when while with xor",
                 First_Symbol       => Tok_Colon,
                 Symbols            =>
                    ": ; , = /= > >= < <= & "
                 & "* / + - ( ) . .. => | "
                 & ":= << >> <>",
                 Identifier_Start   => "abcdefghijklmnopqrstuvwxyz" &
                                       "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                 Identifier_Body    => "abcdefghijklmnopqrstuvwxyz" &
                                       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                       "0123456789" &
                                       "_",
                 Line_Comment_Start => "--",
                 Properties         =>
                   (Case_Sensitive_Identifiers => True,
                    Single_Quote_Token         => True,
                    Ada_Number_Bases           => True,
                    Two_Quote_Escape           => True,
                    Multi_Characters           => False,
                    Multi_Line_Strings         => False));
