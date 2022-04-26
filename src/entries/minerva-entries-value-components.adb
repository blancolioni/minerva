package body Minerva.Entries.Value.Components is

   ------------
   -- Create --
   ------------

   function Create
     (Declaration    : not null access Minerva.Trees.Class;
      Component_Name : Minerva.Names.Minerva_Name;
      Component_Type : Minerva.Types.Class_Reference;
      Word_Offset    : Natural;
      Bit_Offset     : Natural := 0)
      return Constant_Class_Reference
   is
      This : Instance := Instance'
        (Parent with
         Word_Offset => Word_Offset,
         Bit_Offset  => Bit_Offset);
   begin
      Initialize_Value_Entry
        (This               => This,
         Declared_Name      => Component_Name,
         Declaration        => Declaration,
         Entry_Type         => Component_Type);
      return new Instance'(This);
   end Create;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance;
      Unit : in out Tagatha.Code.Instance)
   is
   begin
      Dispatch (This).Push_Address (Unit);
   end Push;

   ------------------
   -- Push_Address --
   ------------------

   overriding procedure Push_Address
     (This : Instance;
      Unit : in out Tagatha.Code.Instance)
   is
      use Minerva.Target;
      Offset : constant Natural :=
                 This.Word_Offset
                   * Natural (Target_Word_Size / 8);
   begin
      Unit.Push
        (Tagatha.Operands.Constant_Operand
           (Tagatha.Tagatha_Integer (Offset)));
      Unit.Operate
        (Tagatha.Op_Add,
         Tagatha.Operands.Take_Address
           (Tagatha.Operands.Type_Annotation
                (This.Data_Type, This.Size)));
   end Push_Address;

end Minerva.Entries.Value.Components;
