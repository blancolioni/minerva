with GCS.Positions;

with Minerva.Parser.Tokens;             use Minerva.Parser.Tokens;
with Minerva.Parser.Lexical;            use Minerva.Parser.Lexical;

with Minerva.Trees.Aspects;
with Minerva.Trees.Expressions;
with Minerva.Trees.Identifiers.Sequence;
with Minerva.Trees.Types;
with Minerva.Trees.Types.Named;

with Minerva.Parser.Aspects;
with Minerva.Parser.Expressions;
with Minerva.Parser.Subprograms;
with Minerva.Parser.Types;

package body Minerva.Parser.Declarations is

   function Parse_Object_Declaration
     (Context : Parser_Context)
     return Minerva.Trees.Declarations.Class_Reference;

   --------------------
   -- At_Declaration --
   --------------------

   function At_Declaration return Boolean is
   begin
      return Tok = Tok_Identifier
        or else Tok = Tok_Function
        or else Tok = Tok_Procedure;
   end At_Declaration;

   -----------------------
   -- Parse_Declaration --
   -----------------------

   function Parse_Declaration
     (Context : Parser_Context)
      return Minerva.Trees.Declarations.Class_Reference
   is
      Declaration : Minerva.Trees.Declarations.Class_Reference;
   begin
      if Tok = Tok_Identifier then
         Declaration := Parse_Object_Declaration (Context);
      elsif Tok = Tok_Function or else Tok = Tok_Procedure then
         Declaration :=
           Minerva.Parser.Subprograms.Parse_Subprogram;
      else
         Declaration := null;
      end if;

      if Tok = Tok_With then
         Scan;

         while Tok = Tok_Identifier loop
            declare
               Aspect : constant Minerva.Trees.Aspects.Class_Reference :=
                          Minerva.Parser.Aspects.Parse_Aspect;
            begin
               Declaration.Add_Aspect (Aspect);
            end;

            if Tok = Tok_Comma then
               Scan;
               if Tok /= Tok_Identifier then
                  Error ("missing aspect");
               end if;
            elsif Tok = Tok_Identifier then
               Error ("missing ','");
            end if;
         end loop;
      end if;

      if Tok = Tok_Semicolon then
         Scan;
      else
         Error ("missing ';'");
      end if;

      return Declaration;

   end Parse_Declaration;

   ---------------------------------------
   -- Parse_Formal_Argument_Declaration --
   ---------------------------------------

   function Parse_Formal_Argument_Declaration
     return Minerva.Trees.Declarations.Objects.Class_Reference
   is
   begin
      return Minerva.Trees.Declarations.Objects.Class_Reference
        (Parse_Object_Declaration (Context => Formal_Argument));
   end Parse_Formal_Argument_Declaration;

   ------------------------------
   -- Parse_Object_Declaration --
   ------------------------------

   function Parse_Object_Declaration
     (Context : Parser_Context)
      return Minerva.Trees.Declarations.Class_Reference
   is
      Position    : constant GCS.Positions.File_Position :=
                      Get_Current_Position;
      Names            : constant Minerva.Trees.Identifiers.Sequence
        .Class_Reference :=
          Minerva.Trees.Identifiers.Sequence.Create_Sequence
            (Position);
      Mode        : Argument_Mode := In_Mode;
      Type_Tree   : Minerva.Trees.Types.Class_Reference;
      Initializer : Minerva.Trees.Expressions.Class_Reference;
   begin
      while Tok = Tok_Identifier loop
         Names.Append
           (Minerva.Trees.Identifiers.Defining_Identifier
              (Get_Current_Position, Tok_Text));
         Scan;
         if Tok = Tok_Comma then
            Scan;
            if Tok /= Tok_Identifier then
               Error ("missing identifier");
            end if;
         elsif Tok = Tok_Identifier then
            Error ("missing ','");
         end if;
      end loop;

      if Tok = Tok_Colon then
         Scan;
      else
         Error ("missing ':'");
      end if;

      if Context = Formal_Argument then
         if Tok = Tok_In then
            Scan;
            if Tok = Tok_Out then
               Scan;
               Mode := In_Out_Mode;
            else
               Mode := In_Mode;
            end if;
         elsif Tok = Tok_Out then
            Mode := Out_Mode;
         end if;
      end if;

      if not Minerva.Parser.Types.At_Type then
         Error ("missing type");
         Type_Tree :=
           Minerva.Trees.Types.Class_Reference
             (Minerva.Trees.Types.Named.Create_Named_Type
                (Get_Current_Position, "integer"));
      else
         Type_Tree := Minerva.Parser.Types.Parse_Type;
      end if;

      if Tok = Tok_Assign then
         Scan;
         if not Minerva.Parser.Expressions.At_Expression then
            Error ("expected an initializer");
         else
            Initializer := Minerva.Parser.Expressions.Parse_Expression;
         end if;
      end if;

      return Minerva.Trees.Declarations.Class_Reference
        ((case Context is
            when Package_Spec | Package_Body | Block =>
              Minerva.Trees.Declarations.Objects
         .Create_Object_Declaration
           (Position    => Position,
            Names       => Names,
            Object_Type => Type_Tree,
            Initializer => Initializer),
            when Formal_Argument                     =>
               Minerva.Trees.Declarations.Objects
         .Create_Formal_Argument_Declaration
           (Position    => Position,
            Names       => Names,
            Mode        => Mode,
            Object_Type => Type_Tree,
            Initializer => Initializer)));

   end Parse_Object_Declaration;

end Minerva.Parser.Declarations;
