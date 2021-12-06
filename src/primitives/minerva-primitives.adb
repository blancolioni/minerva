with GCS.Positions;

with Minerva.Names;

with Minerva.Entries.Types;
with Minerva.Trees.Context.Sequence;
with Minerva.Trees.Declarations.Packages;
with Minerva.Types.Enumerated;

with Minerva.Types.Constructors;
--  with Minerva.Types.Composite.Records;

package body Minerva.Primitives is

   Have_Standard : Boolean := False;
   Standard_Tree : Minerva.Trees.Declarations.Packages.Class_Reference;
   Standard_Env  : Minerva.Environment.Environment_Id;

   Local_Boolean   : Minerva.Types.Class_Reference;
   Local_Character : Minerva.Types.Class_Reference;
   Local_Integer   : Minerva.Types.Class_Reference;
   Local_Address   : Minerva.Types.Class_Reference;

   -----------------------
   -- Create_Primitives --
   -----------------------

   procedure Create_Primitives is
      pragma Assert (not Have_Standard);

      function "+" (S : String) return Minerva.Names.Minerva_Name
      is (Minerva.Names.To_Name (S));

   begin

      Standard_Env := Minerva.Environment.Create_Standard_Environment;

      Standard_Tree :=
        Minerva.Trees.Declarations.Packages.Create_Package_Specification
          (Position      => GCS.Positions.Null_Position,
           Context =>
             Minerva.Trees.Context.Sequence.Create_Sequence
               (GCS.Positions.Null_Position),
           Defining_Name => "standard");

      declare
         use Minerva.Types.Enumerated;
         Boolean_Type  : constant Class_Reference :=
                           Create_Enumerated_Type
                             (Standard_Tree, "boolean");
      begin
         Boolean_Type.Add_Literal (Standard_Tree, "false");
         Boolean_Type.Add_Literal (Standard_Tree, "true");

         Local_Boolean := Minerva.Types.Class_Reference (Boolean_Type);
      end;

      Local_Boolean.Elaborate (Standard_Env);

      declare
         use Minerva.Types.Enumerated;
         Character_Type  : constant Class_Reference :=
                             Create_Enumerated_Type
                               (Standard_Tree, "character");
      begin
         for Ch in Character range ' ' .. '~' loop
            Character_Type.Add_Literal
              (Declaration => Standard_Tree,
               Name        => "'" & Ch & "'",
               Value       => Character'Pos (Ch));
         end loop;

         Local_Character := Minerva.Types.Class_Reference (Character_Type);
      end;

      Local_Character.Elaborate (Standard_Env);

      Local_Integer :=
        Minerva.Types.Constructors.Create_Signed_Integer_Type
          (Definition  => Standard_Tree,
           Name        => "integer",
           First       => -32768,
           Last        => 32767);

      Local_Integer.Elaborate (Standard_Env);

      Have_Standard := True;

      declare
         Byte_Type  : constant Minerva.Types.Class_Reference :=
                        Minerva.Types.Constructors.Create_Modular_Type
                          (Definition => Standard_Tree,
                           Name       => "byte",
                           Modulus    => 256);
         Byte_Entry : constant Minerva.Entries.Types.Class_Reference :=
                        Minerva.Entries.Types.Create
                          (Declaration => Standard_Tree,
                           Name        => +"byte",
                           Definition  => Byte_Type);
      begin
         Minerva.Environment.Insert (Standard_Env, Byte_Entry);
      end;

      declare
         Word_Type  : constant Minerva.Types.Class_Reference :=
                        Minerva.Types.Constructors.Create_Modular_Type
                          (Definition => Standard_Tree,
                           Name       => "word",
                           Modulus    => 65536);
         Word_Entry : constant Minerva.Entries.Types.Class_Reference :=
                        Minerva.Entries.Types.Create
                          (Declaration => Standard_Tree,
                           Name        => +"word",
                           Definition  => Word_Type);
      begin
         Minerva.Environment.Insert (Standard_Env, Word_Entry);
      end;

      Local_Address :=
        Minerva.Types.Constructors.Create_Modular_Type
          (Definition => Standard_Tree,
           Name       => "address",
           Modulus    => 65536);

      declare
         Address_Entry : constant Minerva.Entries.Types.Class_Reference :=
                           Minerva.Entries.Types.Create
                             (Declaration => Standard_Tree,
                              Name        => +"address",
                              Definition  => Local_Address);
      begin
         Minerva.Environment.Insert (Standard_Env, Address_Entry);
      end;

      declare
         Float_Type  : constant Minerva.Types.Class_Reference :=
                         Minerva.Types.Constructors
                           .Single_Precision_Float_Type;
         Float_Entry : constant Minerva.Entries.Types.Class_Reference :=
                         Minerva.Entries.Types.Create
                           (Declaration => Standard_Tree,
                            Name        => +"float",
                            Definition  => Float_Type);
      begin
         Minerva.Environment.Insert (Standard_Env, Float_Entry);
      end;

      declare
         Boolean_Entry : constant Minerva.Entries.Types.Class_Reference :=
                           Minerva.Entries.Types.Create
                             (Declaration => Standard_Tree,
                              Name        =>
                                Minerva.Names.To_Name ("boolean"),
                              Definition  => Local_Boolean);
      begin
         Minerva.Environment.Insert (Standard_Env, Boolean_Entry);
      end;

      declare
         Character_Entry : constant Minerva.Entries.Types.Class_Reference :=
                             Minerva.Entries.Types.Create
                               (Declaration => Standard_Tree,
                                Name        =>
                                  Minerva.Names.To_Name ("character"),
                                Definition  => Local_Character);
      begin
         Minerva.Environment.Insert (Standard_Env, Character_Entry);
      end;

      declare
         Integer_Entry : constant Minerva.Entries.Types.Class_Reference :=
                           Minerva.Entries.Types.Create
                             (Declaration => Package_Standard,
                              Name        => +"integer",
                              Definition  => Local_Integer);
      begin
         Minerva.Environment.Insert (Standard_Env, Integer_Entry);
      end;

      --  declare
      --     Float_Entry  : constant Minerva.Entries.Class_Reference :=
      --                      Minerva.Environment.Get (Standard_Env, "float");
      --     Vector_Env   : constant Minerva.Environment.Environment_Id :=
      --                      Minerva.Environment.Create_Internal_Environment;
      --
      --     procedure Add (Name : String);
      --
      --     ---------
      --     -- Add --
      --     ---------
      --
      --     procedure Add (Name : String) is
      --     begin
      --        Minerva.Environment.Insert
      --          (Environment => Vector_Env,
      --           Element     =>
      --             Minerva.Entries.Value.Components.Create
      --               (Declaration    => Package_Standard,
      --                Component_Name => Minerva.Names.To_Name (Name),
      --                Component_Type => Float_Entry.Entry_Type));
      --     end Add;
      --
      --  begin
      --     Add ("x");
      --     Add ("y");
      --     Add ("z");
      --
      --     declare
      --        use Minerva.Types.Composite.Records;
      --        Vector_Type : constant Class_Reference :=
      --                        Create (Vector_Env);
      --     Vector_Entry : constant Minerva.Entries.Types.Class_Reference :=
      --                         Minerva.Entries.Types.Create
      --                           (Declaration => Package_Standard,
      --                            Name        => +"vector",
      --                            Definition  => Vector_Type);
      --     begin
      --        Insert (Standard_Env, Vector_Entry);
      --     end;
      --  end;

      Have_Standard := True;
   end Create_Primitives;

   ----------------------
   -- Package_Standard --
   ----------------------

   function Package_Standard return Minerva.Trees.Class_Reference is
      pragma Assert (Have_Standard);
   begin
      return Minerva.Trees.Class_Reference (Standard_Tree);
   end Package_Standard;

   ----------------------
   -- Standard_Boolean --
   ----------------------

   function Standard_Boolean return Minerva.Types.Class_Reference is
   begin
      return Local_Boolean;
   end Standard_Boolean;

   ------------------------
   -- Standard_Character --
   ------------------------

   function Standard_Character
     return Minerva.Types.Class_Reference
   is
   begin
      return Local_Character;
   end Standard_Character;

   --------------------------
   -- Standard_Environment --
   --------------------------

   function Standard_Environment
     return Minerva.Environment.Environment_Id
   is
      pragma Assert (Have_Standard);
   begin
      return Standard_Env;
   end Standard_Environment;

   ----------------------
   -- Standard_Integer --
   ----------------------

   function Standard_Integer
     return Minerva.Types.Class_Reference
   is
   begin
      return Local_Integer;
   end Standard_Integer;

   --------------------
   -- System_Address --
   --------------------

   function System_Address
     return Minerva.Types.Class_Reference
   is
   begin
      return Local_Address;
   end System_Address;

end Minerva.Primitives;
