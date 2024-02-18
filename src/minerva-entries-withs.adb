with Minerva.Environment;
with Minerva.Names;

package body Minerva.Entries.Withs is

   -----------------------
   -- Child_Environment --
   -----------------------

   function Child_Environment (This : Class) return Minerva.Ids.Environment_Id
   is
   begin
      return This.Environment;
   end Child_Environment;

   ------------
   -- Create --
   ------------

   function Create
     (Declaration  : not null access Minerva.Trees.Class;
      Package_Name : String;
      Package_Env  : Minerva.Ids.Environment_Id)
      return Constant_Class_Reference
   is
      Result : constant Class_Reference := new Instance;
   begin
      Result.Package_Env := Package_Env;
      Result.Environment :=
        Minerva.Environment.Create_Internal_Environment
          (Minerva.Names.To_Name ("with-" & Package_Name));
      Minerva.Environment.Add_Parent (Result.Environment, Result.Package_Env);

      Result.Initialize_Entry
        (Declared_Name        => Minerva.Names.To_Name (Package_Name),
         Declaration          => Minerva.Trees.Class_Reference (Declaration),
         Is_Package_Reference => True);
      return Constant_Class_Reference (Result);
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
      null;
   end Push_Address;

end Minerva.Entries.Withs;
