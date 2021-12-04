--  with Tagatha.Constants;
--  with Tagatha.Transfers;

with Minerva.Entries.Value.Formal_Arguments;
with Minerva.Entries.Value.Objects;
with Minerva.Names;
with Minerva.Target;
with Minerva.Types;
with Minerva.Values;
--  with Minerva.Values;

package body Minerva.Trees.Declarations.Objects is

   ----------------
   -- Check_Tree --
   ----------------

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      if This.Has_Initializer then
         This.Initializer.Set_Type (This.Object_Type.Get_Type);
      end if;

      Parent (This).Check_Tree (Environment);
   end Check_Tree;

   --------------
   -- Children --
   --------------

   overriding function Children (This : Instance) return Class_Reference_Array
   is
      use type Minerva.Trees.Types.Class_Reference;
   begin
      if This.Has_Initializer then
         return (This.Object_Type.As_Tree, This.Initializer.As_Tree);
      elsif This.Object_Type /= null then
         return (1 => This.Object_Type.As_Tree);
      else
         return Null_Class_Reference_Array;
      end if;
   end Children;

   -------------
   -- Compile --
   -------------

   overriding procedure Compile_Tree
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is

      procedure Compile_Object
        (Name_Node : Minerva.Trees.Identifiers.Class_Reference);

      --------------------
      -- Compile_Object --
      --------------------

      procedure Compile_Object
        (Name_Node : Minerva.Trees.Identifiers.Class_Reference)
      is
         Object_Entry : constant Minerva.Entries.Constant_Class_Reference :=
                          Name_Node.Get_Entry;
      begin
         if Object_Entry.Has_Address then
            if not Object_Entry.Has_Static_Address then
               declare
                  use type Minerva.Trees.Aspects.Class_Reference;
                  subtype Aspect_Reference is
                    Minerva.Trees.Aspects.Class_Reference;
                  subtype Expression_Reference is
                    Minerva.Trees.Expressions.Class_Reference;
                  Address_Aspect : constant Aspect_Reference :=
                                     This.Find_Aspect ("address");
                  Has_Address    : constant Boolean :=
                                     Address_Aspect /= null;
                  Address        : constant Expression_Reference :=
                                     (if Has_Address
                                      then Address_Aspect.Value
                                      else null);
               begin
                  Address.Push (Unit);
                  Object_Entry.Pop (Unit);
               end;
            end if;

         elsif This.Has_Initializer then
            This.Initializer.Push (Unit);
            Object_Entry.Pop (Unit);
         end if;

      end Compile_Object;

   begin
      This.Names.Iterate (Compile_Object'Access);
   end Compile_Tree;

   ----------------------------------------
   -- Create_Formal_Argument_Declaration --
   ----------------------------------------

   function Create_Formal_Argument_Declaration
     (Position    : GCS.Positions.File_Position;
      Names       : Minerva.Trees.Identifiers.Sequence.Class_Reference;
      Mode        : Argument_Mode;
      Object_Type : Minerva.Trees.Types.Class_Reference;
      Initializer : Minerva.Trees.Expressions.Class_Reference)
      return Class_Reference
   is
      use type Minerva.Trees.Expressions.Class_Reference;
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with Names => Names,
         Context           => Formal_Argument_Context,
         Mode              => Mode,
         Object_Type       => Object_Type,
         Has_Initializer   => Initializer /= null,
         Initializer       => Initializer)
      do
         Result.Initialize (Position);
      end return;
   end Create_Formal_Argument_Declaration;

   -------------------------------
   -- Create_Object_Declaration --
   -------------------------------

   function Create_Object_Declaration
     (Position    : GCS.Positions.File_Position;
      Names       : Minerva.Trees.Identifiers.Sequence.Class_Reference;
      Object_Type : Minerva.Trees.Types.Class_Reference;
      Initializer : Minerva.Trees.Expressions.Class_Reference)
      return Class_Reference
   is
      use type Minerva.Trees.Expressions.Class_Reference;
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with Names => Names,
         Context           => Block_Context,
         Mode              => In_Mode,
         Object_Type       => Object_Type,
         Has_Initializer   => Initializer /= null,
         Initializer       => Initializer)
      do
         Result.Initialize (Position);
      end return;
   end Create_Object_Declaration;

   --------------------
   -- Elaborate_Tree --
   --------------------

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
      use type Minerva.Trees.Aspects.Class_Reference;

      Frame_Offset : Natural :=
                       Minerva.Environment.Current_Frame_Offset (Environment);

      procedure Declare_Object
        (Name_Node : Minerva.Trees.Identifiers.Class_Reference);

      --------------------
      -- Declare_Object --
      --------------------

      procedure Declare_Object
        (Name_Node : Minerva.Trees.Identifiers.Class_Reference)
      is
         subtype Object_Reference is
           Minerva.Entries.Value.Objects.Class_Reference;
         Address_Aspect     : constant Minerva.Trees.Aspects.Class_Reference :=
                                This.Find_Aspect ("address");
         Has_Address        : constant Boolean :=
                                Address_Aspect /= null;
         Address            : constant Trees.Expressions.Class_Reference
           := (if Has_Address then Address_Aspect.Value
               else null);
         Has_Static_Address : constant Boolean :=
                                (if Has_Address
                                 then Address.Is_Static (Environment)
                                 else False);
         Static_Address     : constant Minerva.Target.Target_Address_Type :=
                                (if Has_Static_Address
                                 then Minerva.Values.To_Address
                                   (Address.Evaluate (Environment))
                                 else 0);
         Object_Type        : constant Minerva.Types.Class_Reference :=
                                This.Object_Type.Get_Type;
         Name               : constant Minerva.Names.Minerva_Name :=
                                Name_Node.Name;
      begin
         if Minerva.Environment.Exists (Environment, Name, False) then
            This.Add_Error
              ("redeclaration,"
               & Minerva.Names.Cased_Text (Name));
            This.Add_Error
              ("original-declaration,"
               & GCS.Positions.Image
                 (Minerva.Environment.Get (Environment, Name)
                  .Declaration
                  .Source_Position)
               & ","
               & Minerva.Names.Cased_Text (Name));
         elsif Has_Static_Address then
            declare
               New_Entry : constant Object_Reference :=
                             Minerva.Entries.Value.Objects
                               .Create_With_Static_Address
                                 (Declaration => This,
                                  Object_Name => Name,
                                  Object_Type => Object_Type,
                                  Address     => Static_Address);
            begin
               Minerva.Environment.Insert (Environment, New_Entry);
               Name_Node.Set_Entry
                 (Minerva.Entries.Class_Reference (New_Entry));
            end;
         elsif This.Context = Formal_Argument_Context then
            declare
               use Minerva.Entries.Value.Formal_Arguments;
               subtype Formal_Reference is
                 Minerva.Entries.Value.Formal_Arguments
                   .Constant_Class_Reference;
               Frame_Words      : constant Natural :=
                                    This.Object_Type.Get_Type.Size_Words;
               New_Offset       : constant Natural :=
                                    Frame_Offset + Frame_Words;
               New_Entry        : constant Formal_Reference :=
                                    Create
                                      (Declaration    => This,
                                       Argument_Name  => Name,
                                       Argument_Type  => Object_Type,
                                       Mode           => In_Mode,
                                       Is_Aliased     => False,
                                       Null_Exclusion => False,
                                       Offset         =>
                                          Tagatha.Argument_Offset
                                         (New_Offset));
            begin
               Minerva.Environment.Insert (Environment, New_Entry);
               Frame_Offset := New_Offset;
               Name_Node.Set_Entry (New_Entry);
            end;
         else
            declare
               use Minerva.Entries.Value.Objects;
               Frame_Words      : constant Natural :=
                                    This.Object_Type.Get_Type.Size_Words;
               New_Offset       : constant Natural :=
                                    Frame_Offset + Frame_Words;
               New_Entry        : constant Object_Reference :=
                                    (if Has_Address
                                     then Create_With_Dynamic_Address
                                       (Declaration => This,
                                        Object_Name => Name,
                                        Object_Type => Object_Type,
                                        Offset      =>
                                          Tagatha.Local_Offset
                                            (New_Offset),
                                        Address     => Address)
                                     else Create_With_Local_Offset
                                       (Declaration => This,
                                        Object_Name => Name,
                                        Object_Type => Object_Type,
                                        Offset      =>
                                          Tagatha.Local_Offset (New_Offset)));
            begin
               Minerva.Environment.Insert (Environment, New_Entry);
               Frame_Offset := New_Offset;
               Name_Node.Set_Entry (New_Entry);
            end;
         end if;
      end Declare_Object;

   begin
      This.Log
        (Minerva.Environment.Environment_Name (Environment)
         & ": start offset =" & Frame_Offset'Image);
      Parent (This.all).Elaborate_Tree (Environment);
      This.Names.Iterate (Declare_Object'Access);
      This.Log
        (Minerva.Environment.Environment_Name (Environment)
         & ": start offset =" & Frame_Offset'Image);

      Minerva.Environment.Set_Frame_Offset (Environment, Frame_Offset);

   end Elaborate_Tree;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      return "object-declaration";
   end Image;

   ---------------------
   -- Iterate_Entries --
   ---------------------

   overriding procedure Iterate_Entries
     (This    : Instance;
      Process : not null access
        procedure (Item : Minerva.Entries.Constant_Class_Reference))
   is
      procedure Process_Name
        (Name_Node : Minerva.Trees.Identifiers.Class_Reference);

      ------------------
      -- Process_Name --
      ------------------

      procedure Process_Name
        (Name_Node : Minerva.Trees.Identifiers.Class_Reference)
      is
      begin
         Process (Name_Node.Get_Entry);
      end Process_Name;

   begin
      This.Names.Iterate (Process_Name'Access);
   end Iterate_Entries;

end Minerva.Trees.Declarations.Objects;
