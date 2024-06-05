with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;
with Ada.Text_IO;                       use Ada.Text_IO;               

procedure Dining_Philosophers is

    N : constant Positive := 5;                

    protected type Fork is
        entry Grab;
        procedure Put_Down;
    private
        Taken : Boolean := False;
    end Fork;
    protected body Fork is
        entry Grab when not Taken is
        begin
            Taken := True;
        end Grab;
        procedure Put_Down is
        begin
            Taken := False;
        end Put_Down;
    end Fork;

    Number_of_Meals : constant := 20;          

    task type Philosopher (ID : Integer; First, Second : not null access Fork);
    task body Philosopher is
        Seed : Generator;
    begin
        Reset (Seed);

        for Meal in 1..Number_of_Meals loop
            Put_Line ("Filozof" & Integer'Image (ID) & " myśli");
            delay Duration (Random (Seed) * 1.500);

            Put_Line ("Filozof" & Integer'Image (ID) & " jest głodny");
            First.Grab;
            Second.Grab;

            Put_Line ("Filozof" & Integer'Image (ID) & " je");
            delay Duration (Random (Seed) * 1.500);
            Second.Put_Down;
            First.Put_Down;
            
            Put_Line ("Filozof" & Integer'Image (ID) & " skończył jeść");
        end loop;

    end Philosopher;

    Forks : array (Natural range 1..N) of aliased Fork;

    Philosophers : array (Natural range 1..N) of access Philosopher;

begin
    for I in 1..N loop
        if I = N then
            Philosophers(I) := new Philosopher (ID => I, First => Forks(1)'Access, Second => Forks(I)'Access);
        else
            Philosophers(I) := new Philosopher (ID => I, First => Forks(I)'Access, Second => Forks(I+1)'Access);
        end if;
    end loop;
end Dining_Philosophers;