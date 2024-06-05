with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;
with Ada.Real_Time; use Ada.Real_Time;

procedure Dining_Philosophers is

   Number_Of_Philosophers : constant Integer := 5;

   protected type Fork is
      procedure Pick_Up;
      procedure Put_Down;
   private
      Is_Available : Boolean := True;
   end Fork;

   protected body Fork is
      procedure Pick_Up is
      begin
         while not Is_Available loop
            delay 0.001;
         end loop;
         Is_Available := False;
      end Pick_Up;

      procedure Put_Down is
      begin
         Is_Available := True;
      end Put_Down;
   end Fork;

   type Philosopher_Index is range 1 .. Number_Of_Philosophers;

   type Philosopher_Array is array (Philosopher_Index) of Fork;
   Forks : Philosopher_Array;

   task type Philosopher (ID : Philosopher_Index) is
      entry Start;
   end Philosopher;

   task body Philosopher is
      Left_Fork, Right_Fork : Fork renames Forks (ID), Forks ((ID mod Number_Of_Philosophers) + 1);
      package Random_Delay is new Ada.Numerics.Discrete_Random (Duration);
      Gen : Random_Delay.Generator;
   begin
      accept Start;

      loop
         Think;
         Left_Fork.Pick_Up;
         Right_Fork.Pick_Up;
         Eat;
         Right_Fork.Put_Down;
         Left_Fork.Put_Down;
      end loop;
   end Philosopher;

   Philosophers : array (Philosopher_Index) of Philosopher;

   procedure Think (ID : Philosopher_Index) is
      package Random_Delay is new Ada.Numerics.Discrete_Random (Duration);
      Gen : Random_Delay.Generator;
      Delay_Time : Duration;
   begin
      Put_Line ("Philosopher " & ID'Img & " is thinking");
      Random_Delay.Reset (Gen);
      Delay_Time := Random_Delay.Random (Gen) * 1.0;
      delay Delay_Time;
   end Think;

   procedure Eat (ID : Philosopher_Index) is
      package Random_Delay is new Ada.Numerics.Discrete_Random (Duration);
      Gen : Random_Delay.Generator;
      Delay_Time : Duration;
   begin
      Put_Line ("Philosopher " & ID'Img & " is eating");
      Random_Delay.Reset (Gen);
      Delay_Time := Random_Delay.Random (Gen) * 1.0;
      delay Delay_Time;
      Put_Line ("Philosopher " & ID'Img & " finished eating");
   end Eat;

begin
   for I in Philosopher_Index loop
      Philosophers (I).Start;
   end loop;

   delay 20.0;

   for I in Philosopher_Index loop
      abort Philosophers (I);
   end loop;
end Dining_Philosophers;

