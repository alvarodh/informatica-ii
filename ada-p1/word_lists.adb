with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Word_Lists is

	package T_IO renames Ada.Text_IO;

	procedure Free is new Ada.Unchecked_Deallocation(Cell,Word_List_Type);

	procedure Add_Word (List: in out Word_List_Type; Word: in ASU.Unbounded_String) is
		Added: Boolean := False;
		P_Aux: Word_List_Type;	
	begin
		case List = null is
			when True =>
				List := new Cell'(Word,1,null);
			when False =>
				P_Aux := List;
				while P_Aux.Next /= null and not Added loop
					case ASU.To_String(Word) = ASU.To_String(P_Aux.Word) is
						when True =>
							Added := True;
							P_Aux.Count := P_Aux.Count + 1;
						when False =>
							P_Aux := P_Aux.Next;
					end case;
				end loop;
				if not Added then
					P_Aux.Next := new Cell'(Word,1,null);
				end if;
		end case;
	end Add_Word;

	procedure Delete_Word (List: in out Word_List_Type; Word: in ASU.Unbounded_String) is
		Delete: Boolean := False;
		P_Ant: Word_List_Type;
		P_Aux: Word_List_Type;
	begin
		case ASU.To_String(Word) = ASU.To_String(List.Word) is
			when True =>
				P_Aux := List;
				List := List.Next;
				Free(P_Aux);
			when False =>
				P_Aux := List.Next;
				P_Ant := List;
				while P_Aux /= null and not Delete loop
					case ASU.To_String(Word) = ASU.To_String(P_Aux.Word) is
						when True =>
							Delete := True;
							P_Ant.Next := P_Aux.Next;
							Free(P_Aux);
						when False =>
							P_Ant := P_Aux;
							P_Aux := P_Aux.Next;
					end case;
				end loop;
		end case;
	end Delete_Word;

	procedure Search_Word (List: in Word_List_Type; Word: in ASU.Unbounded_String; 
							Count: out Natural) is
		P_Aux: Word_List_Type := List;
		Found: Boolean := False;
	begin
		while P_Aux /= null and not Found loop
			case ASU.To_String(Word) = ASU.To_String(P_Aux.Word) is
				when True =>
					Count := P_Aux.Count;
					Found := True;
				when False =>
					P_Aux := P_Aux.Next;
			end case;
		end loop;
		if not Found then
			Count := 0;
		end if;
	end Search_Word;

	procedure Write_Word (Cell: in Word_List_Type) is
	begin
		T_IO.Put("|" & ASU.To_String(Cell.Word) & "| -");
		T_IO.Put_Line(Natural'Image(Cell.Count));
	end Write_Word;

	procedure Max_Word (List: in Word_List_Type; Word: out ASU.Unbounded_String;
						Count: out Natural) is
		P_Aux: Word_List_Type := List;
		No_Words: Boolean := List.Next = null;
	begin
		Count := 0;
		while P_Aux /= null loop
			if P_Aux.Count > Count then
				Word := P_Aux.Word;
				Count := P_Aux.Count;
			end if;
			P_Aux := P_Aux.Next;
		end loop;
		if No_Words then
			Count := 0;
			Word := ASU.To_Unbounded_String("");
		end if;
	end Max_Word;

	procedure Print_All (List: in Word_List_Type) is
		P_Aux: Word_List_Type := List;
	begin
		if P_Aux.Next = null then
			T_IO.Put_Line("No words");
		else
			while P_Aux /= null loop
				if ASU.To_String(P_Aux.Word) /= "" then
					Write_Word(P_Aux);
				end if;
				P_Aux := P_Aux.Next;
			end loop;
		end if;
	end Print_All;

	procedure Delete_List (List: in out Word_List_Type) is
		P_Aux: Word_List_Type;
	begin
		while List.Next /= null loop
			P_Aux := List;
			List := List.Next;
			Free(P_Aux);
		end loop;
		Free(List);
	end Delete_List;

end Word_Lists;
