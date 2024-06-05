with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Calendar;
with Ada.Real_Time;
with Ada.Numerics.Discrete_Random;
use Ada.Text_IO;
use Ada.Integer_Text_IO;
use Ada.Calendar;
use Ada.Real_Time;

procedure Dining_Philosophers is
   NUM_PHILOSOPHERS : constant Positive := 5;
   TASK_DURATION_MIN : constant Time_Span := Milliseconds (100);
   TASK_DURATION_MAX : constant Time_Span := Milliseconds (1000);

   package Rand_Gen is new Ada.Numerics.Discrete_Random (1 .. 1000);
   use Rand_Gen;
   Gen : Generator;

   type Fork is tagged null record;
   type Fork_Access is access all Fork;
   Forks : array (1 .. NUM_PHILOSOPHERS) of Fork_Access;

   task type Philosopher (Id : Positive);
   task body Philosopher is
      procedure Random_Sleep (Duration_Min, Duration_Max : Time_Span) is
         Start : Time := Clock;
         Duration : Time_Span := Duration_Min + Time_Span'Fraction(Float(Sample(Gen)) / 1000.0) * (Duration_Max - Duration_Min);
      begin
         delay Duration;
      end Random_Sleep;
   begin
      for I in 1 .. 3 loop
         -- Thinking
         Put_Line("Philosopher " & Positive'Image(Id) & " is thinking");
         Random_Sleep(TASK_DURATION_MIN, TASK_DURATION_MAX);

         -- Eating
         if Id = 1 then
            Forks(Id).all.Lock;
            Forks(NUM_PHILOSOPHERS).all.Lock;
         else
            Forks(Id).all.Lock;
            Forks(Id - 1).all.Lock;
         end if;
         Put_Line("Philosopher " & Positive'Image(Id) & " is eating");
         Random_Sleep(TASK_DURATION_MIN, TASK_DURATION_MAX);

         -- Finished eating
         if Id = 1 then
            Forks(NUM_PHILOSOPHERS).all.Unlock;
            Forks(Id).all.Unlock;
         else
            Forks(Id - 1).all.Unlock;
            Forks(Id).all.Unlock;
         end if;
         Put_Line("Philosopher " & Positive'Image(Id) & " finished eating");
      end loop;
   end Philosopher;

   task type Fork is
      entry Lock;
      entry Unlock;
   end Fork;

   task body Fork is
   begin
      loop
         accept Lock;
         accept Unlock;
      end loop;
   end Fork;

   Philosopher_Tasks : array (1 .. NUM_PHILOSOPHERS) of Philosopher := (others => Philosopher'(Id => <>));

begin
   -- Initialize forks
   for I in Forks'Range loop
      Forks(I) := new Fork;
   end loop;

   -- Wait for all philosophers to finish
   delay 10.0;
end Dining_Philosophers;
