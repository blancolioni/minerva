with Minerva.Target;
with Minerva.Types.Universal;

package body Minerva.Types.Modular is

   ------------
   -- Create --
   ------------

   function Create
     (Definition : not null access Minerva.Trees.Class;
      Name       : String;
      Modulus    : Positive)
      return Class_Reference
   is
      Bits : Natural := 0;
      Result : Instance := Instance'
        (Parent with
         Modulus =>
           Minerva.Values.Unsigned_Integer_Value
             (Minerva.Types.Universal.Universal_Integer,
              Modulus));
   begin

      while 2 ** Bits < Modulus loop
         Bits := Bits + 1;
      end loop;

      Result.Definition := Minerva.Trees.Class_Reference (Definition);
      Result.Object_Size := Minerva.Target.Object_Size (Bits);
      Result.Name := Minerva.Names.To_Name (Name);
      Result.Is_Integral := True;
      Result.Is_Numeric := True;
      Result.Is_Ordered := True;
      Result.Is_Signed := False;
      return new Instance'(Result);
   end Create;

end Minerva.Types.Modular;
