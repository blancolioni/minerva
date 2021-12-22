with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Doubly_Linked_Lists;

with Minerva.Logging;

with Minerva.Operators;

with Minerva.Entries.Value.Formal_Arguments;
with Minerva.Entries.Subprograms;
with Minerva.Entries.Withs;
with Minerva.Types;
with Minerva.Types.Callable;

with Minerva.Primitives;

package body Minerva.Environment is

   Have_Standard : Boolean := False;
   Standard_Env  : Environment_Id;

   package Entry_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Minerva.Entries.Constant_Class_Reference, Minerva.Entries."=");

   package Environment_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Minerva.Ids.Environment_Id, Minerva.Ids."=");

   function Find
     (List : Entry_Lists.List;
      Name : Minerva.Names.Minerva_Name)
      return Entry_Lists.Cursor;

   function Find
     (Environment : Minerva.Ids.Environment_Id;
      Name        : Minerva.Names.Minerva_Name)
      return Entry_Lists.Cursor;

   type Environment_Record is
      record
         Name         : Minerva.Names.Minerva_Name;
         Parent       : Minerva.Ids.Environment_Id_Base;
         Used         : Environment_Lists.List;
         Entries      : Entry_Lists.List;
         Frame_Offset : Natural := 0;
      end record;

   package Environment_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Minerva.Ids.Environment_Id, Environment_Record);

   Environment_Table : Environment_Vectors.Vector;

   procedure Create_Operators
     (Environment : Minerva.Ids.Environment_Id;
      For_Type    : Minerva.Types.Class_Reference);

   --  function Add
   --    (Rec : Environment_Record)
   --     return Minerva.Ids.Environment_Id;
   --
   --  function Add
   --    (Rec : Environment_Record)
   --     return Minerva.Ids.Environment_Id
   --  is
   --  begin
   --  end Add;

   procedure Log
     (Environment : Environment_Id;
      Message     : String)
     with Unreferenced;

   ----------------
   -- Add_Parent --
   ----------------

   procedure Add_Parent
     (This   : Environment_Id;
      Parent : Environment_Id)
   is
      pragma Assert
        (Minerva.Ids."=" (Environment_Table (This).Parent,
         Minerva.Ids.Null_Environment_Id));
   begin
      Environment_Table (This).Parent := Parent;
   end Add_Parent;

   ------------------------
   -- Create_Environment --
   ------------------------

   function Create_Environment
     (Name   : Minerva.Names.Minerva_Name;
      Parent : Minerva.Ids.Environment_Id)
      return Minerva.Ids.Environment_Id
   is
   begin
      Environment_Table.Append
        (Environment_Record'
           (Name         => Name,
            Parent       => Parent,
            others       => <>));
      return Environment_Table.Last_Index;
   end Create_Environment;

   ---------------------------------
   -- Create_Internal_Environment --
   ---------------------------------

   function Create_Internal_Environment
     (Name   : Minerva.Names.Minerva_Name)
      return Minerva.Ids.Environment_Id
   is
   begin
      Environment_Table.Append
        (Environment_Record'
           (Name         => Name,
            Parent       => Minerva.Ids.Null_Environment_Id,
            others       => <>));
      return Environment_Table.Last_Index;
   end Create_Internal_Environment;

   ----------------------
   -- Create_Operators --
   ----------------------

   procedure Create_Operators
     (Environment : Minerva.Ids.Environment_Id;
      For_Type    : Minerva.Types.Class_Reference)
   is
      use Minerva.Operators;
      use Minerva.Primitives;

      subtype Call_Reference is
        Minerva.Types.Callable.Class_Reference;

      ------------
      -- Formal --
      ------------

      function Formal
        (Argument_Name : String;
         Argument_Type : Minerva.Types.Class_Reference)
         return Minerva.Entries.Value.Formal_Arguments.Constant_Class_Reference
      is (Minerva.Entries.Value.Formal_Arguments.Create
          (Declaration    => Package_Standard,
           Argument_Name  => Minerva.Names.To_Name (Argument_Name),
           Argument_Type  => Argument_Type,
           Mode           => In_Mode,
           Is_Aliased     => False,
           Null_Exclusion => False,
           Offset         => 1));

      Boolean_Type : constant Minerva.Types.Class_Reference :=
                       Minerva.Primitives.Standard_Boolean;

      A_A_Boolean  : constant Call_Reference :=
                       Minerva.Types.Callable.Create
                         (Definition       => Package_Standard,
                          Return_Type      => Boolean_Type,
                          Formal_Arguments =>
                            (Formal ("Left", For_Type),
                             Formal ("Right", For_Type)));
      A_A_A        : constant Call_Reference :=
                       Minerva.Types.Callable.Create
                         (Definition       => Package_Standard,
                          Return_Type      => For_Type,
                          Formal_Arguments =>
                            (Formal ("Left", For_Type),
                             Formal ("Right", For_Type)));

      procedure Add_Operator
        (Name        : String;
         Operator    : Minerva_Operator;
         Call_Type   : Call_Reference);

      ------------------
      -- Add_Operator --
      ------------------

      procedure Add_Operator
        (Name        : String;
         Operator    : Minerva_Operator;
         Call_Type   : Call_Reference)
      is
         pragma Unreferenced (Name);
         Op : constant Minerva.Entries.Subprograms.Class_Reference :=
                Minerva.Entries.Subprograms.Create_Operator_Function
                  (Declaration      => Package_Standard,
                   Operator         => Operator,
                   Environment_Name =>
                     Minerva.Environment.Environment_Name (Environment),
                   Call_Type        => Call_Type);
      begin
         Op.Set_Intrinsic;
         Insert (Environment, Op);
      end Add_Operator;

   begin
      if not For_Type.Is_Limited then
         Add_Operator ("=", Op_EQ, A_A_Boolean);
         Add_Operator ("/=", Op_NE, A_A_Boolean);
      end if;

      if For_Type.Is_Ordered then
         Add_Operator (">", Op_GT, A_A_Boolean);
         Add_Operator ("<", Op_LT, A_A_Boolean);
         Add_Operator (">=", Op_GE, A_A_Boolean);
         Add_Operator ("<=", Op_LE, A_A_Boolean);
      end if;

      if For_Type.Is_Numeric then
         Add_Operator ("+", Op_Add, A_A_A);
         Add_Operator ("-", Op_Subtract, A_A_A);
         Add_Operator ("*", Op_Multiply, A_A_A);
         Add_Operator ("/", Op_Divide, A_A_A);
      end if;

   end Create_Operators;

   ---------------------------------
   -- Create_Standard_Environment --
   ---------------------------------

   function Create_Standard_Environment
     return Minerva.Ids.Environment_Id
   is
      pragma Assert (not Have_Standard);
      use type Minerva.Ids.Environment_Id_Base;
      Env : constant Environment_Record := Environment_Record'
        (Name         => Minerva.Names.To_Name ("standard"),
         Parent       => Minerva.Ids.Null_Environment_Id,
         Used         => <>,
         Entries      => <>,
         Frame_Offset => 0);
   begin
      Environment_Table.Append (Env);
      Standard_Env := Environment_Table.Last_Index;
      Have_Standard := True;
      return Standard_Env;
   end Create_Standard_Environment;

   --------------------------
   -- Current_Frame_Offset --
   --------------------------

   function Current_Frame_Offset
     (Environment : Minerva.Ids.Environment_Id) return Natural
   is
   begin
      return Environment_Table (Environment).Frame_Offset;
   end Current_Frame_Offset;

   ----------------------
   -- Environment_Name --
   ----------------------

   function Environment_Name
     (Environment : Minerva.Ids.Environment_Id)
      return String
   is
   begin
      return Minerva.Names.Cased_Text
        (Environment_Table (Environment).Name);
   end Environment_Name;

   ----------------------
   -- Environment_Name --
   ----------------------

   function Environment_Name
     (Environment : Minerva.Ids.Environment_Id)
      return Minerva.Names.Minerva_Name
   is
      use Minerva.Ids;
      Base_Name : constant Minerva.Names.Minerva_Name :=
                    Environment_Table (Environment).Name;
   begin
      if Environment = 1 then
         return Minerva.Names.Null_Minerva_Name;
      elsif Parent (Environment) = Minerva.Ids.Null_Environment_Id then
         return Base_Name;
      else
         return Minerva.Names.Join
           (Environment_Name (Parent (Environment)), Base_Name);
      end if;
   end Environment_Name;

   ------------
   -- Exists --
   ------------

   function Exists
     (Environment : Minerva.Ids.Environment_Id;
      Name        : Minerva.Names.Minerva_Name)
      return Boolean
   is
   begin
      return Entry_Lists.Has_Element (Find (Environment, Name));
   end Exists;

   ------------
   -- Exists --
   ------------

   function Exists
     (Environment : Minerva.Ids.Environment_Id;
      Name        : String)
      return Boolean
   is
   begin
      return Exists
        (Environment, Minerva.Names.To_Name (Name));
   end Exists;

   --------------------
   -- Exists_Locally --
   --------------------

   function Exists_Locally
     (Environment : Minerva.Ids.Environment_Id;
      Name        : Minerva.Names.Minerva_Name)
      return Boolean
   is
   begin
      return Entry_Lists.Has_Element
        (Find (Environment_Table (Environment).Entries, Name));
   end Exists_Locally;

   ----------
   -- Find --
   ----------

   function Find
     (List : Entry_Lists.List;
      Name : Minerva.Names.Minerva_Name)
      return Entry_Lists.Cursor
   is
      use type Minerva.Names.Minerva_Name;
   begin
      for Position in List.Iterate loop
         if Entry_Lists.Element (Position).Name = Name then
            return Position;
         end if;
      end loop;
      return Entry_Lists.No_Element;
   end Find;

   ----------
   -- Find --
   ----------

   function Find
     (Environment : Minerva.Ids.Environment_Id;
      Name        : Minerva.Names.Minerva_Name)
      return Entry_Lists.Cursor
   is
      use Minerva.Ids;
      Env       : Minerva.Ids.Environment_Id := Environment;
      Base_Name : constant Minerva.Names.Minerva_Name :=
                    Minerva.Names.Base_Name (Name);

   begin
      for Prefix of Minerva.Names.Qualifiers (Name) loop
         declare
            Prefix_Position : constant Entry_Lists.Cursor :=
                                Find (Env, Prefix);
         begin
            if not Entry_Lists.Has_Element (Prefix_Position) then
               return Entry_Lists.No_Element;
            end if;

            declare
               Prefix_Entry : constant Entries.Constant_Class_Reference :=
                                Entry_Lists.Element (Prefix_Position);
            begin
               if not Prefix_Entry.Is_Package_Reference then
                  return Entry_Lists.No_Element;
               end if;
               Env :=
                 Entries.Withs.Constant_Class_Reference (Prefix_Entry)
                 .Child_Environment;
            end;
         end;
      end loop;

      loop
         declare
            Rec      : Environment_Record renames
                         Environment_Table (Env);
            Position : constant Entry_Lists.Cursor :=
                         Find (Rec.Entries, Base_Name);
         begin
            if Entry_Lists.Has_Element (Position) then
               return Position;
            else
               for E of Rec.Used loop
                  declare
                     Use_Position : constant Entry_Lists.Cursor :=
                                      Find (E, Base_Name);
                  begin
                     if Entry_Lists.Has_Element (Use_Position) then
                        return Use_Position;
                     end if;
                  end;
               end loop;

               if Rec.Parent /= Null_Environment_Id then
                  Env := Rec.Parent;
               else
                  return Entry_Lists.No_Element;
               end if;
            end if;
         end;
      end loop;

   end Find;

   ---------
   -- Get --
   ---------

   function Get
     (Environment : Minerva.Ids.Environment_Id;
      Name        : Minerva.Names.Minerva_Name)
      return Minerva.Entries.Constant_Class_Reference
   is
      Position : constant Entry_Lists.Cursor :=
                   Find (Environment, Name);
      pragma Assert (Entry_Lists.Has_Element (Position),
                     "violated precondition");
   begin
      return Entry_Lists.Element (Position);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Environment : Minerva.Ids.Environment_Id;
      Name        : String)
      return Minerva.Entries.Constant_Class_Reference
   is
   begin
      return Get
        (Environment, Minerva.Names.To_Name (Name));
   end Get;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Environment : Minerva.Ids.Environment_Id;
      Element     : not null access constant Minerva.Entries.Instance'Class)
   is
   begin
      Environment_Table (Environment).Entries.Append
        (Minerva.Entries.Constant_Class_Reference (Element));
      if Element.Is_Type_Entry then
         Create_Operators (Environment, Element.Entry_Type);
      end if;
   end Insert;

   ---------------------
   -- Iterate_Matches --
   ---------------------

   procedure Iterate_Matches
     (Environment : Minerva.Ids.Environment_Id;
      Name        : Minerva.Names.Minerva_Name;
      Matches     : not null access
        function (Test_Entry : Minerva.Entries.Constant_Class_Reference)
      return Boolean;
      Process     : not null access
        procedure (Found_Entry : Minerva.Entries.Constant_Class_Reference))
   is
      Checked : Environment_Lists.List;

      procedure Iterate (Env : Minerva.Ids.Environment_Id);

      -------------
      -- Iterate --
      -------------

      procedure Iterate (Env : Minerva.Ids.Environment_Id) is
         use Minerva.Ids;
         use type Minerva.Names.Minerva_Name;
         Rec : Environment_Record renames
                 Environment_Table (Env);
      begin
         if Checked.Contains (Env) then
            return;
         end if;

         Checked.Append (Env);
         for Item of Rec.Entries loop
            if Item.Name = Name
              and then Matches (Item)
            then
               Process (Item);
            end if;
         end loop;

         if Rec.Parent /= Null_Environment_Id then
            Iterate (Rec.Parent);
         end if;

         for E of Rec.Used loop
            Iterate (E);
         end loop;
      end Iterate;

   begin
      Iterate (Environment);
   end Iterate_Matches;

   -------------------
   -- Iterate_Names --
   -------------------

   procedure Iterate_Names
     (Environment : Minerva.Ids.Environment_Id;
      Name        : Minerva.Names.Minerva_Name;
      Process     : not null access
        procedure (Found_Entry : Minerva.Entries.Constant_Class_Reference))
   is
      use Minerva.Ids;
      use type Minerva.Names.Minerva_Name;
      Env      : Environment_Record renames
                   Environment_Table (Environment);
   begin
      for Item of Env.Entries loop
         if Item.Name = Name then
            Process (Item);
         end if;
      end loop;
      if Env.Parent /= Null_Environment_Id then
         Iterate_Names (Env.Parent, Name, Process);
      end if;
   end Iterate_Names;

   ---------
   -- Log --
   ---------

   procedure Log
     (Environment : Environment_Id;
      Message     : String)
   is
   begin
      Minerva.Logging.Log
        (Category => Environment_Name (Environment),
         Message  => Message);
   end Log;

   ------------
   -- Parent --
   ------------

   function Parent
     (Environment : Minerva.Ids.Environment_Id)
      return Minerva.Ids.Environment_Id_Base
   is
   begin
      return Environment_Table (Environment).Parent;
   end Parent;

   ----------------------
   -- Set_Frame_Offset --
   ----------------------

   procedure Set_Frame_Offset
     (Environment : Minerva.Ids.Environment_Id;
      New_Offset  : Natural)
   is
   begin
      Environment_Table (Environment).Frame_Offset := New_Offset;
   end Set_Frame_Offset;

   ---------------------
   -- Use_Environment --
   ---------------------

   procedure Use_Environment
     (Environment      : Environment_Id;
      Used_Environment : Environment_Id)
   is
   begin
      Environment_Table (Environment).Used.Append (Used_Environment);
   end Use_Environment;

end Minerva.Environment;
