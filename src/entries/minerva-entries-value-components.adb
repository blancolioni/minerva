package body Minerva.Entries.Value.Components is

   ------------
   -- Create --
   ------------

   function Create
     (Declaration    : not null access Minerva.Trees.Class;
      Component_Name : Minerva.Names.Minerva_Name;
      Component_Type : Minerva.Types.Class_Reference)
      return Class_Reference
   is
   begin
      return This : constant Class_Reference := new Instance do
         This.Initialize_Value_Entry
           (Declared_Name      => Component_Name,
            Declaration        => Declaration,
            Entry_Type         => Component_Type);
      end return;
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
      Parent (This).Push_Address (Unit);
      Unit.Push
        (Tagatha.Tagatha_Integer
           (Tagatha.Size_Octets (This.Component_Offset)));
      Unit.Operate (Tagatha.Op_Add);
   end Push_Address;

end Minerva.Entries.Value.Components;
