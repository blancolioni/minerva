package body Minerva.Types.Records is

   ------------
   -- Create --
   ------------

   function Create
     (Definition            : not null access Minerva.Trees.Class;
      Component_Environment : Minerva.Environment.Environment_Id)
      return Class_Reference
   is
      Result : Instance;
   begin
      Result.Initialize
        (Definition,
         Minerva.Target.To_Object_Size
           (Minerva.Environment.Current_Frame_Offset (Component_Environment)),
         Component_Environment);
      return new Instance'(Result);
   end Create;

end Minerva.Types.Records;
