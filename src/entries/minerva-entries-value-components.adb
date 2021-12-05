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

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      This.Push_Address (Unit);
      Unit.Store;
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      This.Push_Address (Unit);
      Unit.Dereference;
   end Push;

   ------------------
   -- Push_Address --
   ------------------

   overriding procedure Push_Address
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      Unit.Push
        (Tagatha.Tagatha_Integer (This.Word_Offset));
      Unit.Operate (Tagatha.Op_Add);
   end Push_Address;

end Minerva.Entries.Value.Components;
