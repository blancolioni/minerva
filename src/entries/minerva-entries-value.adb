package body Minerva.Entries.Value is

   ---------------
   -- Data_Type --
   ---------------

   function Data_Type (This : Class) return Tagatha.Tagatha_Data_Type is
      use type Minerva.Types.Class_Reference;
   begin
      if This.Entry_Type /= null
        and then This.Entry_Type.Is_Floating_Point
      then
         return Tagatha.Floating_Point_Data;
      else
         return Tagatha.Untyped_Data;
      end if;
   end Data_Type;

   ----------------------------
   -- Initialize_Value_Entry --
   ----------------------------

   procedure Initialize_Value_Entry
     (This               : in out Instance'Class;
      Declared_Name      : Minerva.Names.Minerva_Name;
      Declaration        : not null access Minerva.Trees.Class;
      Entry_Type         : Minerva.Types.Class_Reference;
      Initializer        : Minerva.Trees.Expressions.Class_Reference := null;
      Has_Address        : Boolean := False;
      Has_Static_Address : Boolean := False)
   is
      use type Minerva.Trees.Expressions.Class_Reference;
   begin
      This.Initialize_Entry
        (Declared_Name      => Declared_Name,
         Declaration        => Declaration,
         Entry_Type         => Entry_Type,
         Initializer        => Initializer,
         Has_Address        => Has_Address,
         Has_Static_Address => Has_Static_Address,
         Has_Initializer    => Initializer /= null);
   end Initialize_Value_Entry;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      raise Constraint_Error with "unimplemented pop of base value type";
   end Pop;

   ------------------
   -- Push_Address --
   ------------------

   overriding procedure Push_Address
     (This : Instance; Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      raise Constraint_Error
        with "unimplemented push address of base value type";
   end Push_Address;

   ----------
   -- Size --
   ----------

   function Size (This : Class) return Tagatha.Tagatha_Size is
      Size_Bits : constant Natural := This.Entry_Type.Size_Bits;
   begin
      return Tagatha.Bits_To_Size (Size_Bits);
   end Size;

end Minerva.Entries.Value;
