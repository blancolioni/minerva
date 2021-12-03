with Minerva.Target;
with Minerva.Types.Universal;

package body Minerva.Types.Signed is

   ------------
   -- Create --
   ------------

   function Create
     (Definition : not null access Minerva.Trees.Class;
      Name       : String;
      First, Last : Integer)
      return Class_Reference
   is
      Bits : Natural := 0;
      Domain : constant Natural :=
                 Integer'Max (Last - First + 1, 0);
      Result : Instance := Instance'
        (Parent with
         First =>
           Minerva.Values.Signed_Integer_Value
             (Minerva.Types.Universal.Universal_Integer,
              First),
         Last        =>
           Minerva.Values.Signed_Integer_Value
             (Minerva.Types.Universal.Universal_Integer,
              First));
   begin

      while 2 ** Bits < Domain loop
         Bits := Bits + 1;
      end loop;

      Result.Definition := Minerva.Trees.Class_Reference (Definition);
      Result.Object_Size := Minerva.Target.Object_Size (Bits);
      Result.Name := Minerva.Names.To_Name (Name);
      Result.Is_Integral := True;
      Result.Is_Numeric := True;
      Result.Is_Ordered := True;
      Result.Is_Signed := True;
      return new Instance'(Result);
   end Create;

end Minerva.Types.Signed;
