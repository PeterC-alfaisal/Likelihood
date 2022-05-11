
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"rows","title":"Rows","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"the variable to use as the rows in the contingency table (not necessary when providing a formula, see the examples)\n"}},{"name":"cols","title":"Columns","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"the variable to use as the columns in the contingency table (not necessary when providing a formula, see the examples)\n"}},{"name":"counts","title":"Counts (optional)","type":"Variable","suggested":["continuous"],"permitted":["numeric"],"default":null,"description":{"R":"the variable to use as the counts in the contingency table (not necessary when providing a formula, see the examples)\n"}},{"name":"varA","title":"Variance analysis","type":"Bool","default":false},{"name":"pcRow","title":"Row","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide row percentages\n"}},{"name":"pcCol","title":"Column","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide column percentages\n"}},{"name":"barplot","title":"Bar Plot","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), show barplots\n"}},{"name":"yaxis","title":"Y-axis","type":"List","options":[{"name":"ycounts","title":"Counts"},{"name":"ypc","title":"Percentages"}],"default":"ycounts","description":{"R":"ycounts (default) or ypc. Use respectively `counts` or `percentages` for the bar plot y-axis\n"}},{"name":"yaxisPc","title":"","type":"List","options":[{"name":"total_pc","title":"of total"},{"name":"column_pc","title":"within column"},{"name":"row_pc","title":"within rows"}],"default":"total_pc","description":{"R":"total_pc (default), column_pc, or row_pc. Use respectively percentages `of total`, `within columns`, or `within rows` for the bar plot y-axis.\n"}},{"name":"xaxis","title":"X-axis","type":"List","options":[{"name":"xrows","title":"Rows"},{"name":"xcols","title":"Columns"}],"default":"xrows","description":{"R":"rows (default), or columns in bar plot X axis\n"}},{"name":"bartype","title":"Bar Type","type":"List","options":[{"name":"dodge","title":"Side by side"},{"name":"stack","title":"Stacked"}],"default":"dodge","description":{"R":"stack or side by side (default), barplot type\n"}}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Large Contingency Table Test",
    jus: "3.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			typeName: 'VariableSupplier',
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Rows",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "rows",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Columns",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "cols",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Counts",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "counts",
							maxItemCount: 1,
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					cell: {"column":0,"row":0},
					stretchFactor: 1,
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Additional Analysis",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "varA",
									verticalAlignment: "center",
									controls: [

									]
								}
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			cell: {"row":1,"column":1},
			stretchFactor: 1,
			margin: "large",
			controls: [
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Percentages",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "pcRow"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "pcCol"
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Plots",
			stretchFactor: 1,
			collapsed: true,
			controls: [
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					margin: "large",
					stretchFactor: 1,
					controls: [
						{
							type: DefaultControls.LayoutBox,
							typeName: 'LayoutBox',
							cell: {"column":0,"row":0},
							stretchFactor: 1,
							controls: [
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: "Plots",
									fitToGrid: true,
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "barplot"
										}
									]
								},
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: "Bar Type",
									fitToGrid: true,
									controls: [
										{
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											name: "dodge",
											optionName: "bartype",
											optionPart: "dodge"
										},
										{
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											name: "stack",
											optionName: "bartype",
											optionPart: "stack"
										}
									]
								}
							]
						},
						{
							type: DefaultControls.LayoutBox,
							typeName: 'LayoutBox',
							cell: {"column":1,"row":0},
							stretchFactor: 1,
							controls: [
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: "Y-Axis",
									fitToGrid: true,
									controls: [
										{
											name: "ycounts",
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											optionName: "yaxis",
											optionPart: "ycounts"
										},
										{
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											name: "ypc",
											optionName: "yaxis",
											optionPart: "ypc",
											style: "inline",
											verticalAlignment: "center",
											controls: [
												{
													type: DefaultControls.ComboBox,
													typeName: 'ComboBox',
													name: "yaxisPc",
													enable: "(ypc)"
												}
											]
										}
									]
								},
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: "X-Axis",
									fitToGrid: true,
									controls: [
										{
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											name: "xrows",
											optionName: "xaxis",
											optionPart: "xrows"
										},
										{
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											name: "xcols",
											optionName: "xaxis",
											optionPart: "xcols"
										}
									]
								}
							]
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
