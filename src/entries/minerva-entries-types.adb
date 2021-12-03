package body Minerva.Entries.Types is

   ------------
   -- Create --
   ------------

   function Create
     (Declaration : not null access Minerva.Trees.Class;
      Name        : Minerva.Names.Minerva_Name;
      Definition  : not null access constant Minerva.Types.Class)
      return Class_Reference
   is
   begin
      return This : constant Class_Reference := new Instance'
        (Parent with Type_Definition =>
           Minerva.Types.Class_Reference (Definition))
      do
         Minerva.Entries.Initialize_Entry
           (This               => Parent (This.all),
            Declared_Name      => Name,
            Declaration        => Declaration,
            Is_Type_Entry      => True);
         This.Entry_Type := Minerva.Types.Class_Reference (Definition);
      end return;
   end Create;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Instance; Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      raise Constraint_Error with
        "cannot pop a type entry: " & This.Cased_Text;
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance; Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      raise Constraint_Error with
        "cannot push a type entry: " & This.Cased_Text;
   end Push;

   ------------------
   -- Push_Address --
   ------------------

   overriding procedure Push_Address
     (This : Instance; Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      raise Constraint_Error with
        "cannot push a type entry address: " & This.Cased_Text;
   end Push_Address;

end Minerva.Entries.Types;
