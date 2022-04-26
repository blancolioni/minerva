with Ada.Strings.Fixed;

package body Minerva.Entries.Value.Objects is

   ---------------------------------
   -- Create_With_Dynamic_Address --
   ---------------------------------

   function Create_With_Dynamic_Address
     (Declaration : not null access Minerva.Trees.Class;
      Object_Name : Minerva.Names.Minerva_Name;
      Object_Type : Minerva.Types.Class_Reference;
      Offset      : Tagatha.Local_Offset;
      Address     : Minerva.Trees.Expressions.Class_Reference)
      return Class_Reference
   is
   begin
      return This : constant Class_Reference := new Instance'
        (Parent with
           Address        => Address,
         Static_Address => 0,
         Local          => Offset,
         Variable_State => new Boolean'(False))
      do
         This.Initialize_Value_Entry
           (Declared_Name      => Object_Name,
            Declaration        => Declaration,
            Entry_Type         => Object_Type,
            Has_Address        => True,
            Has_Static_Address => False);
      end return;
   end Create_With_Dynamic_Address;

   ------------------------------
   -- Create_With_Local_Offset --
   ------------------------------

   function Create_With_Local_Offset
     (Declaration : not null access Minerva.Trees.Class;
      Object_Name : Minerva.Names.Minerva_Name;
      Object_Type : Minerva.Types.Class_Reference;
      Offset      : Tagatha.Local_Offset)
      return Class_Reference
   is
   begin
      return This : constant Class_Reference := new Instance'
        (Parent with
           Address          => null,
         Static_Address   => 0,
         Local            => Offset,
         Variable_State   => null)
      do
         This.Initialize_Value_Entry
           (Declared_Name      => Object_Name,
            Declaration        => Declaration,
            Entry_Type         => Object_Type);
      end return;
   end Create_With_Local_Offset;

   --------------------------------
   -- Create_With_Static_Address --
   --------------------------------

   function Create_With_Static_Address
     (Declaration : not null access Minerva.Trees.Class;
      Object_Name : Minerva.Names.Minerva_Name;
      Object_Type : Minerva.Types.Class_Reference;
      Address     : Minerva.Target.Target_Address_Type)
      return Class_Reference
   is
   begin
      return This : constant Class_Reference := new Instance'
        (Parent with
           Address        => null,
         Static_Address => Address,
         Local            => 1,
         Variable_State   => null)
      do
         This.Initialize_Value_Entry
           (Declared_Name      => Object_Name,
            Declaration        => Declaration,
            Entry_Type         => Object_Type,
            Has_Address        => True,
            Has_Static_Address => True);
      end return;
   end Create_With_Static_Address;

   overriding procedure Push_Address
     (This : Instance; Unit : in out Tagatha.Code.Instance)
   is
      use Tagatha.Operands;
   begin
      if This.Has_Address and then not This.Has_Static_Address
        and then not This.Variable_State.all
      then
         Unit.Push
           (Take_Address (Dispatch (This).To_Operand));
      else
         Parent (This).Push_Address (Unit);
      end if;
   end Push_Address;

   ----------------
   -- To_Operand --
   ----------------

   overriding function To_Operand
     (This : Instance)
      return Tagatha.Operands.Operand_Type
   is
   begin
      if This.Has_Static_Address then
         return Tagatha.Operands.External_Operand
           (Ada.Strings.Fixed.Trim
              (This.Static_Address'Image, Ada.Strings.Left),
           Absolute => True);
      elsif This.Has_Address then
         if This.Variable_State.all then
            return Tagatha.Operands.Indirect
              (Tagatha.Operands.Local_Operand
                 (This.Local));
         else
            This.Variable_State.all := True;
            return Tagatha.Operands.Local_Operand (This.Local);
         end if;
      else
         return Tagatha.Operands.Local_Operand (This.Local);
      end if;
   end To_Operand;

   ------------------
   -- Push_Address --
   ------------------

   --  overriding procedure Push_Address
   --    (This : Instance; Unit : in out Tagatha.Code.Instance)
   --  is
   --  begin
   --     if This.Has_Static_Address then
   --        Unit.Push
   --          (Value => Tagatha.Tagatha_Integer (This.Static_Address),
   --           Size  => Tagatha.Default_Address_Size);
   --     elsif This.Has_Address then
   --        if This.Variable_State.all then
   --           Unit.Push_Local
   --             (Offset => This.Local,
   --              Data   => Tagatha.Address_Data);
   --        else
   --           Unit.Push_Local_Address
   --             (Offset => This.Local);
   --           This.Variable_State.all := True;
   --        end if;
   --     else
   --        Unit.Push_Local_Address
   --          (Offset => This.Local);
   --     end if;
   --  end Push_Address;

   -----------
   -- Store --
   -----------

   --  overriding procedure Store
   --    (This  : Instance;
   --     Value : not null access constant
   --       Minerva.Trees.Expressions.Instance'Class;
   --     Unit  : in out Tagatha.Code.Instance)
   --  is
   --  begin
   --     if This.Has_Address
   --       and then not This.Has_Static_Address
   --       and then not This.Variable_State.all
   --     then
   --        Unit.Push_Local_Address
   --          (Offset => This.Local);
   --        Value.Push (Unit);
   --        Unit.Store (Tagatha.Address_Data, Tagatha.Default_Address_Size);
   --        This.Variable_State.all := True;
   --     else
   --        Parent (This).Store (Value, Unit);
   --     end if;
   --  end Store;

end Minerva.Entries.Value.Objects;
