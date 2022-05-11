
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"rows","title":"Rows","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"the variable to use as the rows in the contingency table (not necessary when providing a formula, see the examples)\n"}},{"name":"cols","title":"Columns","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"the variable to use as the columns in the contingency table (not necessary when providing a formula, see the examples)\n"}},{"name":"counts","title":"Counts (optional)","type":"Variable","suggested":["continuous"],"permitted":["numeric"],"default":null,"description":{"R":"the variable to use as the counts in the contingency table (not necessary when providing a formula, see the examples)\n"}},{"name":"pll","title":"Likelihood function","type":"Bool","default":false},{"name":"nul","title":"Null hypothesis Odds","type":"Number","min":1e-9,"max":100000000000,"default":1},{"name":"alt","title":"Alternative hypothesis Odds","type":"Number","min":1e-9,"max":100000000000,"default":1},{"name":"lint","type":"Number","title":"Likelihood interval support level","min":1,"max":10,"default":2},{"name":"ciWidth","title":"Likelihood-based confidence interval","type":"Number","min":50,"max":99.9,"default":95,"description":{"R":"a number between 50 and 99.9 (default: 95), width of the confidence intervals to provide\n"}},{"name":"varA","title":"Variance analysis","type":"Bool","default":false},{"name":"pcRow","title":"Row","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide row percentages\n"}},{"name":"pcCol","title":"Column","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide column percentages\n"}}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "McNemar Paired Test",
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
							label: "Settings",
							controls: [
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									margin: "large",
									controls: [
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "nul",
											format: FormatDef.number
										},
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "alt",
											format: FormatDef.number
										},
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "lint",
											format: FormatDef.number
										},
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "ciWidth",
											suffix: "%",
											format: FormatDef.number
										},
										{
											type: DefaultControls.LayoutBox,
											typeName: 'LayoutBox',
											margin: "large",
											controls: [
												{
													type: DefaultControls.CheckBox,
													typeName: 'CheckBox',
													name: "pll"
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
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			cell: {"column":2,"row":1},
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
							verticalAlignment: "center"
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			cell: {"row":2,"column":1},
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
		}
	]
});

module.exports = { view : view, options: options };
