with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Strings.Maps;
with Ada.Characters.Handling;
with Word_Lists;

procedure Words is

	package T_IO renames Ada.Text_IO;
	package ASU renames Ada.Strings.Unbounded;
	package ACL renames Ada.Command_Line;
	package E_IO renames Ada.IO_Exceptions;
	package ASM renames Ada.Strings.Maps;
	package ACH renames Ada.Characters.Handling;

	Interactive_Command: String := "-i";
	Bad_Chars: String := (" .,?!/\()[]&%$=+*:;#@~{}");
	Usage_Error: Exception;

	procedure Separate_Words (Line: in out ASU.Unbounded_String; Finish: out Boolean;
							Word: out ASU.Unbounded_String) is
		Len_Line: Natural;
		Len_Word: Natural;
		Bad_Char_In_Word: Boolean;
	begin
		Len_Word := ASU.Index(Line," ");
		Len_Line := ASU.Length(Line);
		begin
			Word := ASU.Head(Line,Len_Word - 1);
		exception
			when Constraint_Error =>
				Word := Line;
				Finish := True;
		end;
		Bad_Char_In_Word := ASU.Index(Word,ASM.To_Set(Bad_Chars)) /= 0;
		while Bad_Char_In_Word loop
			Len_Word := ASU.Length(Word);
			if ASU.Index(Word,ASM.To_Set(Bad_Chars)) = Len_Word then
				ASU.Head(Word,Len_Word - 1);
			else
				ASU.Tail(Word,Len_Word - 1);
			end if;
			Bad_Char_In_Word := ASU.Index(Word,ASM.To_Set(Bad_Chars)) /= 0;
		end loop;
		ASU.Tail(Line,Len_Line - Len_Word);
	end Separate_Words;

	procedure Create_List_From_File (List: out Word_Lists.Word_List_Type;
									File: T_IO.File_Type) is
		Line: ASU.Unbounded_String;
		Word: ASU.Unbounded_String;
		End_Of_Line: Boolean := False;
	begin
		while not T_IO.End_Of_File(File) loop
			Line := ASU.To_Unbounded_String(ACH.To_Lower(T_IO.Get_Line(File)));
			while not End_Of_Line loop
				Separate_Words(Line,End_Of_Line,Word);
				if ASU.To_String(Word) /= "" then
					Word_Lists.Add_Word(List,Word);
				end if;
			end loop;
			End_Of_Line := False;
		end loop;
	end Create_List_From_File;

	procedure Create_List (List: out Word_Lists.Word_List_Type) is
		File: T_IO.File_Type;
	begin
		begin
			T_IO.Open(File,T_IO.In_File,ACL.Argument(1));
			Create_List_From_File(List,File);
			T_IO.Close(File);
		exception
			when E_IO.Name_Error =>
				raise Word_Lists.Word_List_Error;
		end;
	end Create_List;

	procedure Not_Interactive_Form is
		List: Word_Lists.Word_List_Type;
		Word: ASU.Unbounded_String;
		Count: Natural;
	begin
		List := new Word_Lists.Cell;
		Create_List(List);
		Word_Lists.Max_Word(List,Word,Count);
		if Count /= 0 then
			T_IO.Put("The most frequent word: |");
			T_IO.Put(ASU.To_String(Word) & "| -");
			T_IO.Put_Line(Natural'Image(Count));
		else
			T_IO.Put_Line("No words");
		end if;
		Word_Lists.Delete_List(List);
	end Not_Interactive_Form;

	procedure Options_Menu is
	begin
		T_IO.Put_Line("Options");
		T_IO.Put_Line("1 Add word");
		T_IO.Put_Line("2 Delete word");
		T_IO.Put_Line("3 Search word");
		T_IO.Put_Line("4 Print All");
		T_IO.Put_Line("5 Quit");
		T_IO.New_Line;
		T_IO.Put("Your option? ");
	end Options_Menu;

	procedure Word_Lists_Option (list: in out Word_Lists.Word_List_Type; 
								Number: in Integer; End_Program: in out Boolean) is
		Word: ASU.Unbounded_String;
		Line: ASU.Unbounded_String;
		Count: Natural;
		End_Of_Line: Boolean := False;
	begin
		case Number is
			when 1 =>
				T_IO.Put("Word? ");
				Line := ASU.To_Unbounded_String(ACH.To_Lower(T_IO.Get_Line));
				T_IO.New_Line;
				while not End_Of_Line loop
					Separate_Words(Line,End_Of_Line,Word);
					if ASU.To_String(Word) /= "" then 
						Word_Lists.Add_Word(List,Word);
						T_IO.Put_Line("Word |" & ASU.To_String(Word) & "| added");
						T_IO.New_Line;
					end if;
				end loop;
				End_Of_Line := False;
			when 2 =>
				T_IO.Put("Word? ");
				Line := ASU.To_Unbounded_String(ACH.To_Lower(T_IO.Get_Line));
				T_IO.New_Line;
				while not End_Of_Line loop
					Separate_Words(Line,End_Of_Line,Word);
					if ASU.To_String(Word) /= "" then 
						Word_Lists.Delete_Word(List,Word);
						T_IO.Put_Line("Word |" & ASU.To_String(Word) & "| deleted");
						T_IO.New_Line;
					end if;
				end loop;
				End_Of_Line := False;
			when 3 =>
				T_IO.Put("Word? ");
				Line := ASU.To_Unbounded_String(ACH.To_Lower(T_IO.Get_Line));
				T_IO.New_Line;
				while not End_Of_Line loop
					Separate_Words(Line,End_Of_Line,Word);
					if ASU.To_String(Word) /= "" then 
						Word_Lists.Search_Word(List,Word,Count);
						if Count /= 0 then
							T_IO.Put("|" & ASU.To_String(Word) & "| -");
							T_IO.Put_Line(Natural'Image(Count));
						else
							T_IO.Put("Word |" & ASU.To_String(Word));
							T_IO.Put_Line("| not found");
						end if;
						T_IO.New_Line;
					end if;
				end loop;
				End_Of_Line := False;
			when 4 =>
				T_IO.New_Line;
				Word_Lists.Print_All(List);
				T_IO.New_Line;
			when 5 =>
				T_IO.New_Line;
				End_Program := True;
				Word_Lists.Max_Word(List,Word,Count);
				if Count /= 0 then
					T_IO.Put("The most frequent word: |" & ASU.To_String(Word));
					T_IO.Put_Line("| -" & Natural'Image(Count));
				else
					T_IO.Put_Line("No words");
				end if;
			when others =>
				null;
		end case;
	end Word_Lists_Option;

	procedure Write_File_Not_Found is
		Start_Loop: Integer;
	begin
		if ACL.Argument(1) = Interactive_Command then
			Start_Loop := 2;
		else
			Start_Loop := 1;
		end if;
		for I in Start_Loop..ACL.Argument_Count loop
			T_IO.Put(ACL.Argument(I));
			if I /= ACL.Argument_Count then
				T_IO.Put(" ");
			end if;
		end loop;
		T_IO.Put_Line(": file not found");
	end Write_File_Not_Found;

	procedure Elect_Options (List: in out Word_Lists.Word_List_Type;
							File_Name: ASU.Unbounded_String) is
		Number: Integer := 0;
		Bad_Number: ASU.Unbounded_String;
		End_Program: Boolean := False;
	begin
		while not End_Program loop
			if ASU.To_String(File_Name) /= "" then
				T_IO.Put_Line("Words from file: " & ASU.To_String(File_Name));
			else
				Write_File_Not_Found;
			end if;
			Options_Menu;
			begin
				Bad_Number := ASU.To_Unbounded_String(T_IO.Get_Line);
				Number := Integer'Value(ASU.To_String(Bad_Number));
				Word_Lists_Option(List,Number,End_Program);
			exception
				when Constraint_Error =>
					T_IO.Put_Line("No option for " & ASU.To_String(Bad_Number));
					T_IO.New_Line;
			end;
		end loop;
	end Elect_Options;

	procedure Interactive_Form is
		List: Word_Lists.Word_List_Type;
		File: T_IO.File_Type;
		File_Name: ASU.Unbounded_String;
	begin
		begin
			T_IO.Open(File,T_IO.In_File,ACL.Argument(2));
			File_Name := ASU.To_Unbounded_String(ACL.Argument(2));
			List := new Word_Lists.Cell;
			Create_List_From_File(List,File);
			T_IO.Close(File);
		exception
			when E_IO.Name_Error =>
				List := new Word_Lists.Cell;
				File_Name := ASU.To_Unbounded_String("");
		end;
		Elect_Options(List,File_Name);
		Word_Lists.Delete_List(List);
	end Interactive_Form;

	procedure Elect_Form is
	begin
		case ACL.Argument_Count is
			when 0 =>
				raise Usage_Error;
			when 1 =>
				if ACL.Argument(1) /= Interactive_Command then
					Not_Interactive_Form;
				else
					raise Usage_Error;
				end if;
			when others =>
				if ACL.Argument(1) = Interactive_Command then
					Interactive_Form;
				else
					raise Word_Lists.Word_List_Error;
				end if;
		end case;
	end Elect_Form;

begin

	Elect_Form;
	exception
		when Usage_Error =>
			T_IO.Put_Line("Usage: ./words [" & Interactive_Command & "] <file_name>");
		when Word_Lists.Word_List_Error =>
			Write_File_Not_Found;

end Words;
