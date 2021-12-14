with GCS.Positions;

with Minerva.Parser.Tokens;             use Minerva.Parser.Tokens;
with Minerva.Parser.Lexical;            use Minerva.Parser.Lexical;

with Minerva.Parser.Blocks;
with Minerva.Parser.Declarations;
with Minerva.Parser.Identifiers;
with Minerva.Parser.Types;

with Minerva.Trees.Blocks;
with Minerva.Trees.Context.Sequence;
with Minerva.Trees.Declarations.Objects.Sequence;
with Minerva.Trees.Declarations.Packages;
with Minerva.Trees.Declarations.Subprograms;
with Minerva.Trees.Types;

package body Minerva.Parser.Subprograms is

   function Parse_Context
     return Minerva.Trees.Context.Sequence.Class_Reference;

   function Parse_Subprogram
     (Context : Minerva.Trees.Context.Sequence.Class_Reference)
     return Minerva.Trees.Class_Reference;

   function Parse_Package
     (Context : Minerva.Trees.Context.Sequence.Class_Reference)
      return Minerva.Trees.Class_Reference;

   procedure Parse_Formal_Arguments
     (This : Minerva.Trees.Declarations.Objects.Sequence.Class_Reference);

   ----------------------------
   -- Parse_Compilation_Unit --
   ----------------------------

   function Parse_Compilation_Unit
     return Minerva.Trees.Class_Reference
   is
      Context : constant Minerva.Trees.Context.Sequence.Class_Reference :=
                  Parse_Context;
      Result  : Minerva.Trees.Class_Reference;
   begin
      if Tok = Tok_Procedure or else Tok = Tok_Function then
         Result := Parse_Subprogram (Context);
      elsif Tok = Tok_Package then
         Result := Parse_Package (Context);
      else
         Error ("expected a compilation unit");
      end if;

      if Tok = Tok_Semicolon then
         Scan;
      else
         Error ("missing ';'");
      end if;

      return Result;

   end Parse_Compilation_Unit;

   -------------------
   -- Parse_Context --
   -------------------

   function Parse_Context
     return Minerva.Trees.Context.Sequence.Class_Reference
   is
      Context : constant Minerva.Trees.Context.Sequence.Class_Reference :=
                  Minerva.Trees.Context.Sequence.Create_Sequence
                    (GCS.Positions.Get_Current_Position);
   begin
      while Tok = Tok_With or else Tok = Tok_Use loop
         declare
            Is_With : constant Boolean := Tok = Tok_With;
         begin
            Scan;
            while Tok = Tok_Identifier loop
               declare
                  Pos  : constant GCS.Positions.File_Position :=
                           GCS.Positions.Get_Current_Position;
                  Name : constant String :=
                           Minerva.Parser.Identifiers
                             .Parse_Qualified_Identifier;
               begin
                  if Name /= "" then
                     Context.Append
                       (if Is_With
                        then Minerva.Trees.Context.Create_With_Context
                          (Pos, Name)
                        else Minerva.Trees.Context.Create_Use_Context
                          (Pos, Name));
                  end if;
                  if Tok = Tok_Comma then
                     if Next_Tok /= Tok_Identifier then
                        Error ("identifier expected");
                     end if;
                     Scan;
                  elsif Tok = Tok_Identifier then
                     Error ("missing ','");
                  end if;
               end;
            end loop;

            if Tok /= Tok_Semicolon then
               Error ("missing ';'");
            else
               Scan;
            end if;
         end;
      end loop;

      return Context;
   end Parse_Context;

   ----------------------------
   -- Parse_Formal_Arguments --
   ----------------------------

   procedure Parse_Formal_Arguments
     (This : Minerva.Trees.Declarations.Objects.Sequence.Class_Reference)
   is
   begin
      pragma Assert (Tok = Tok_Left_Paren);
      Scan;
      while Tok = Tok_Identifier loop
         declare
            subtype Argument_Reference is
              Minerva.Trees.Declarations.Objects.Class_Reference;
            use type Minerva.Trees.Declarations.Objects.Class_Reference;
            Argument : constant Argument_Reference :=
                         Minerva.Parser.Declarations
                           .Parse_Formal_Argument_Declaration;
         begin
            if Argument /= null then
               This.Append (Argument);
            end if;
         end;
         if Tok = Tok_Semicolon then
            Scan;
         elsif Tok = Tok_Identifier then
            Error ("missing ';'");
         elsif Tok /= Tok_Right_Paren then
            Error ("missing ')'");
            Skip_To ((Tok_Semicolon, Tok_Return, Tok_Is));
            return;
         end if;
      end loop;

      if Tok = Tok_Right_Paren then
         Scan;
      else
         Error ("expected ')'");
      end if;
   end Parse_Formal_Arguments;

   -------------------
   -- Parse_Package --
   -------------------

   function Parse_Package
     (Context : Minerva.Trees.Context.Sequence.Class_Reference)
      return Minerva.Trees.Class_Reference
   is
      Is_Body : constant Boolean := Next_Tok = Tok_Body;
   begin
      pragma Assert (Tok = Tok_Package);
      Scan;

      if Tok = Tok_Body then
         Scan;
      end if;

      if Tok /= Tok_Identifier then
         if Tok in Tok_Reserved_Word then
            Error ("'" & Tok_Text & "' cannot be used as a package name");
         else
            Error ("missing package name");
         end if;
         return null;
      end if;

      declare
         use Minerva.Trees.Declarations.Packages;
         Start : constant GCS.Positions.File_Position :=
                   Get_Current_Position;
         Name  : constant String :=
                   Minerva.Parser.Identifiers.Parse_Qualified_Identifier;
         Tree  : constant Class_Reference :=
                   (if Is_Body
                    then Create_Package_Body (Start, Context, Name)
                    else Create_Package_Specification
                      (Start, Context, Name));
         Context : constant Parser_Context :=
                     (if Is_Body then Package_Body else Package_Spec);
      begin
         if Tok = Tok_Is then
            Scan;
         else
            Error ("missing 'is'");
         end if;

         while Declarations.At_Declaration loop
            Tree.Append (Declarations.Parse_Declaration (Context));
         end loop;

         if Tok = Tok_End then
            Scan;
         else
            Error ("missing 'end'");
         end if;

         if Tok = Tok_Identifier then
            declare
               End_Name : constant String :=
                            Minerva.Parser.Identifiers
                              .Parse_Qualified_Identifier;
            begin
               if End_Name /= Name then
                  Error ("end " & Name
                         & " required");
                  Error ("found " & End_Name);
               end if;
            end;
         else
            Error ("end " & Name
                      & " required");
         end if;

         return Minerva.Trees.Class_Reference (Tree);
      end;

   end Parse_Package;

   ----------------------
   -- Parse_Subprogram --
   ----------------------

   function Parse_Subprogram
     (Context : Minerva.Trees.Context.Sequence.Class_Reference)
      return Minerva.Trees.Class_Reference
   is
      use type Minerva.Trees.Context.Sequence.Class_Reference;
      subtype Sequence_Reference is
        Minerva.Trees.Declarations.Objects.Sequence.Class_Reference;

      Is_Function : constant Boolean := Tok = Tok_Function;
      Arguments   : Sequence_Reference;
      Return_Type : Minerva.Trees.Types.Class_Reference;
      Body_Block  : Minerva.Trees.Blocks.Class_Reference;
   begin
      pragma Assert (Tok in Tok_Function | Tok_Procedure);
      Scan;

      if Tok /= Tok_Identifier and then Tok /= Tok_String_Constant then
         Error ("expected a name");
         return null;
      end if;

      if Tok = Tok_String_Constant then
         if not Is_Function then
            Error ("operators-must-be-functions");
         end if;
         if Context /= null then
            Error ("operators-cannot-be-top-level");
         end if;
      end if;

      declare
         Start : constant GCS.Positions.File_Position :=
                   Get_Current_Position;
         Name  : constant String :=
                   (if Context = null
                    then Tok_Text
                    else Identifiers.Parse_Qualified_Identifier);
      begin
         if Context = null then
            pragma Assert
              (Tok = Tok_Identifier or else Tok = Tok_String_Constant);
            Scan;
         end if;

         Arguments :=
           Minerva.Trees.Declarations.Objects.Sequence.Create_Sequence
             (Get_Current_Position);

         if Tok = Tok_Left_Paren then
            Parse_Formal_Arguments (Arguments);
         end if;

         if Is_Function then
            if Tok = Tok_Return then
               Scan;
               Return_Type := Minerva.Parser.Types.Parse_Type;
            else
               Error ("missing return type");
            end if;
         end if;

         if Tok = Tok_Is then
            Scan;
            Body_Block := Minerva.Parser.Blocks.Parse_Block (Name);
         end if;

         return Minerva.Trees.Class_Reference
           (Minerva.Trees.Declarations.Subprograms.Create_Declaration
              (Position         => Start,
               Context          => Context,
               Defining_Name    => Name,
               Formal_Arguments => Arguments,
               Return_Type      => Return_Type,
               Subprogram_Body  => Body_Block));
      end;

   end Parse_Subprogram;

   ----------------------
   -- Parse_Subprogram --
   ----------------------

   function Parse_Subprogram
     return Minerva.Trees.Declarations.Class_Reference
   is
   begin
      return Minerva.Trees.Declarations.Class_Reference
        (Parse_Subprogram (null));
   end Parse_Subprogram;

end Minerva.Parser.Subprograms;
