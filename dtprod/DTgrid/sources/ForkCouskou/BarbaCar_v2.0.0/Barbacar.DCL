// Barbacar.dcl
// Boite de dialogue création de carroyage (Barbacar.lsp)

Bar_info_button : button {
	key = "button_BARCADAGEO";
	label = "CadaGeo Info...";
}

Bar_ok_cancel_info_button : column {
	: row {
		fixed_width = true;
		alignment = centered;
		ok_button;
		: spacer { width = 2; }
		cancel_button;
		: spacer { width = 2; }
		Bar_info_button;
	}
}


barbacar : dialog
		{
			label = "Barbacar v2.0.0 - création de carroyage";
			initial_focus = "popup_list_BARLAYER";
			alignment=left;
			fixed_width=true;
   : spacer { width = 4; }
   :boxed_column
    {
    label="Sélection contour par...";
    :row
    {
    	fixed_width = true;
    	alignment = centered;
    	:button {key = "button_BAROBJECT"; label = "Contour rectangulaire...";}
	spacer_1;
	:button {key = "button_BARPOINT"; label = "3 points...";}
    }
    }
    : spacer { width = 4; }
    :boxed_column
    {
    label="Calques des entités créées";
    : popup_list {key = "popup_list_BARLAYER"; label = "Calque :"; alignment=left; edit_width = 15;}
    }
    : spacer { width = 4; }
    :boxed_column
    {
    label="Styles des textes";
    : popup_list {key = "popup_list_BARSTYLE"; label = "Style :"; alignment=left; edit_width = 15;}
    }
    : spacer { width = 4; }
    :boxed_column
    {
    label="Echelle du plan";
    : popup_list {key = "popup_list_BARSCALE"; label = "Echelle :"; edit_width = 15;}
    }
    : spacer { width = 4; }
    :boxed_column
    {
    label="Symboles flèche NORD";
    : popup_list {key = "popup_list_BARBLOCK"; label = "Flèche NORD :"; edit_width = 15;}
    }
    : spacer { width = 4; }
    : column
    {
	spacer_1;
	Bar_ok_cancel_info_button;
	}
    : spacer { width = 4; }	
}

