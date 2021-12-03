package body Minerva.Trees.Statements.Null_Statement is

   ------------
   -- Create --
   ------------

   function Create
     (Position : GCS.Positions.File_Position)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance do
         Result.Initialize (Position);
      end return;
   end Create;

end Minerva.Trees.Statements.Null_Statement;
