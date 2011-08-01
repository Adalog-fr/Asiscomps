procedure Tse_Essai is
   I : Integer;
   I1 : constant := 10;
   I2 : constant := 2*I1;
   I3 : constant Integer := 2**3;
   S1 : constant String := "A""B";
   S2 : constant String := S1 & Tse_Essai.S1;
   type Enum is (First, Second, Third);
   E1 : Enum;
   E2 : constant Enum := Second;

   subtype Constrained is Integer range -10 .. +10;
   subtype Unconstrained is Integer;
   type Modular is mod 5;
   subtype Submodular is Modular range 1..3;

   type Der1 is new Integer range -2..100;
   type Der2 is new Der1 range -1..40;
   type Der3 is new Der1;

   type Arr1 is array (Der1, Der2 range 5..30) of Integer;
   type Arr2 is array (Der1 range <>, Der2 range <>) of Integer;

   A1 : Arr1;
   A2 : Arr2 (I1 .. I2-1, Der2'First +1 .. Der2'Last - 1);
   A3 : Arr2 (A2'Range (1), A2'Range (2));

   type Arr3 is array (Enum) of Integer;
   type Arr4 is array (Enum range Second..Second) of Integer;
   type Arr5 is array (1..10) of Integer;
   type Arr6 is array (Arr1'Range(1)) of Integer;

    A4 : array (Enum) of Integer;
    A5 : array (Enum range Second..Second) of Integer;
    A6 : array (1..10) of Integer;
    A7 : array (Arr1'Range(1)) of Integer;

    type Rec is
       record
          F1 : Arr1;
       end record;
    R1 : Rec;

begin
   I := 1;
   I := Integer'Pred ((1+2)*3 + (2));
   I := Integer'Succ (2*I1 + I2 + I3);
   I := Integer'First;
   I := Integer'Last;
   I := Constrained'First;
   I := Constrained'Last;
   I := Unconstrained'First;
   I := Unconstrained'Last;
   I := Integer (Modular'First);
   I := Integer (Modular'Last);
   I := Integer (Submodular'First);
   I := Integer (Submodular'Last);
   I := Character'Pos ('A');

   I := Der1'Pos (Der1'First);
   I := Der1'Pos (Der1'Last);
   I := Der2'Pos (Der2'First);
   I := Der2'Pos (Der2'Last);
   I := Der3'Pos (Der3'First);
   I := Der3'Pos (Der3'Last);

   I := Der1'Pos (Arr1'First (1));
   I := Der1'Pos (Arr1'Last (1));
   I := Der2'Pos (Arr1'First (1+1));
   I := Der2'Pos (Arr1'Last (2));

   I := Der1'Pos (A1'First (1));
   I := Der1'Pos (A1'Last (1));
   I := Der2'Pos (A1'First (1+1));
   I := Der2'Pos (A1'Last (2));

   I := Der1'Pos (A2'First (1));
   I := Der1'Pos (A2'Last (1));
   I := Der2'Pos (A2'First (1+1));
   I := Der2'Pos (A2'Last (2));

   I := Der1'Pos (A3'First (1));
   I := Der1'Pos (A3'Last (1));
   I := Der2'Pos (A3'First (1+1));
   I := Der2'Pos (A3'Last (2));

   E1 := First;
   E1 := Enum'Succ (E2);
   E1 := Enum'First;
   E1 := Enum'Last;
   E1 := Arr3'First;
   E1 := Arr3'Last;
   E1 := Arr4'First;
   E1 := Arr4'Last;
   I  := Arr5'First;
   I  := Arr5'Last;
   I  := Integer (Arr6'First);
   I  := Integer (Arr6'Last);

   E1 := A4'First;
   E1 := A4'Last;
   E1 := A5'First;
   E1 := A5'Last;
   I  := A6'First;
   I  := A6'Last;
   I  := Integer (A7'First);
   I  := Integer (A7'Last);

   I := Der1'Pos (R1.F1'First (1));
   I := Der1'Pos (R1.F1'Last (1));

   declare
      procedure P1 (P1 : Arr1) is
      begin
         I := Der1'Pos (P1'First (1));
         I := Der1'Pos (P1'Last (1));
      end P1;
      generic
         Form : in out Arr1;
      procedure Gen;
      procedure Gen is
      begin
         I := Der1'Pos (Form'First (1));
         I := Der1'Pos (Form'Last (1));
      end Gen;
   begin
      null;
   end;
end Tse_Essai;
