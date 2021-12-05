package body Minerva.Types.Records is

   ------------
   -- Create --
   ------------

   function Create
     (Definition            : not null access Minerva.Trees.Class;
      Component_Environment : Minerva.Environment.Environment_Id)
      return Class_Reference
   is
      Result : Instance := Instance'
        (Parent with Components => Component_Environment);
   begin
      Result.Definition := Minerva.Trees.Class_Reference (Definition);
      Result.Object_Size :=
        Minerva.Target.To_Object_Size
          (Minerva.Environment.Current_Frame_Offset (Component_Environment));
      return new Instance'(Result);
   end Create;

end Minerva.Types.Records;
