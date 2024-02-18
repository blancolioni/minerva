package body Minerva.Entries.Packages is

   ------------
   -- Create --
   ------------

   function Create
     (Declaration    : not null access Minerva.Trees.Class;
      Package_Name   : Minerva.Names.Minerva_Name;
      Container_Name : Minerva.Names.Minerva_Name)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance do
         Result.Initialize_Entry
           (Declared_Name      => Package_Name,
            Declaration        => Minerva.Trees.Class_Reference (Declaration),
            Is_Package_Entry   => True);
         Result.Parent_Name := Container_Name;
      end return;
   end Create;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Instance; Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      raise Constraint_Error with "cannot pop a package";
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance; Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      raise Constraint_Error with "cannot push a package";
   end Push;

   ------------------
   -- Push_Address --
   ------------------

   overriding procedure Push_Address
     (This : Instance; Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      raise Constraint_Error with "cannot push a package address";
   end Push_Address;

end Minerva.Entries.Packages;
