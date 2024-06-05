with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

procedure Filozofowie is

   -- Liczba filozofów
   N : constant Positive := 5;

   type Filozof_Id is range 1 .. N;

   -- Typ widelca, każdy widelec jest task'em kontrolowanym synchronicznie
   task type Widelec is
      entry Podnies;
      entry Odloz;
   end Widelec;

   task body Widelec is
      available : Synchronous_Task_Control.Suspension_Object;
   begin
      Loop
         accept Podnies do
            Set_False(available);
         end Podnies;

         Set_True(available);
         Accept Odloz;
      end Loop;
   end Widelec;

   -- Tablica widelców
   Widelce : array (Filozof_Id) of Widelec;

   -- Typ task'a filozofa
   task type Filozof(F : Filozof_Id);

   task body Filozof is
      procedure Mysl is
         use Ada.Real_Time;
         use Ada.Text_IO;
      begin
         Put_Line("Filozof" & F'Img & " myśli.");
         delay until Clock + Milliseconds(500 + Integer'Mod(F * 1000, 500));
      end Mysl;

      procedure Jedz is
         use Ada.Real_Time;
         use Ada.Text_IO;
      begin
         Put_Line("Filozof" & F'Img & " je.");
         delay until Clock + Milliseconds(500 + Integer'Mod(F * 1000, 500));
      end Jedz;

   begin
      loop
         Mysl;
         Widelce(F).Podnies;
         Widelce((F mod N) + 1).Podnies;

         Jedz;

         Widelce(F).Odloz;
         Widelce((F mod N) + 1).Odloz;
      end loop;
   end Filozof;

   -- Tablica filozofów
   Filozofowie : array (Filozof_Id) of Filozof(Filozof_Id'(1)) := (
      (Filozof_Id'(1)), (Filozof_Id'(2)), (Filozof_Id'(3)), (Filozof_Id'(4)), (Filozof_Id'(5))
   );

begin
   null;
end Filozofowie;