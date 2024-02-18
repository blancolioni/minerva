package body Minerva.Entries.Value is

   -------------
   -- Content --
   -------------

   function Content (This : Class) return Data_Type is
      use type Minerva.Types.Class_Reference;
   begin
      if This.Entry_Type /= null
        and then This.Entry_Type.Is_Floating_Point
      then
         return Floating_Point_Data;
      else
         return Untyped_Data;
      end if;
   end Content;

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
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      raise Constraint_Error with "unimplemented pop of base value type";
   end Pop;

   ------------------
   -- Push_Address --
   ------------------

   overriding procedure Push_Address
     (This : Instance; Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      raise Constraint_Error
        with "unimplemented push address of base value type";
   end Push_Address;

   ---------------
   -- Size_Bits --
   ---------------

   function Size_Bits (This : Class) return Size_Bits_Range is
   begin
      return This.Entry_Type.Size_Bits;
   end Size_Bits;

end Minerva.Entries.Value;
