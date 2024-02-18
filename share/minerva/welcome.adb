procedure Welcome is

   Background_Color : Word_8;
   for Background_Color'Address use 53281;
   
   Border_Color : Word_8;
   for Background_Color'Address use 53280;
   
   Screen : array (1 .. 1000) of Storage_Element;
   for Screen'Address use 1024;
   
   Color  : array (1 .. 1000) of Storage_Element;
   for Color'Address use 55296;

begin

   Background_Color := 6;
   Border_Color := 14;
   
   Screen := (others => 32);
   Color := (others => 14);
   
end Welcome;

   