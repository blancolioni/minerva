package body Minerva.Types.Void is

   subtype Parent is Minerva.Types.Instance;

   type Instance is new Parent with null record;

   overriding function Description
     (This : Instance)
      return String
   is ("void");

   Local_Void_Type : aliased constant Instance := (others => <>);

   function Void_Type return Class_Reference
   is (Local_Void_Type'Access);

end Minerva.Types.Void;
