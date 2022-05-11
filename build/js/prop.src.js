
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"var","title":"Variable","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"]},{"name":"counts","title":"Counts (optional)","type":"Variable","default":null,"permitted":["numeric"],"description":{"R":"the counts in `data`\n"}},{"name":"expected","title":"Expected counts","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), whether expected counts should be displayed\n"}},{"name":"bi","title":"Binomial","type":"Bool","default":false},{"name":"ciWidth","title":"Likelihood-based confidence interval","type":"Number","min":50,"max":99.9,"default":95,"description":{"R":"a number between 50 and 99.9 (default: 95), width of the confidence intervals to provide\n"}},{"name":"lint","type":"Number","title":"Likelihood interval support level","min":1,"max":10,"default":2},{"name":"pll","title":"Likelihood function","type":"Bool","default":false},{"name":"ratio","title":"Expected Proportions for Alt. H","type":"Array","template":{"type":"Number","min":0,"default":1},"default":null,"description":{"R":"a vector of numbers: the expected proportions\n"}},{"name":"varA","title":"Variance analysis","type":"Bool","default":false}];

const view = function() {
    
    

    View.extend({
        jus: "2.0",

        events: [

	],

	update: require('./prop.events').update,

	remoteDataChanged: require('./prop.events').onRemoteDataChanged

    }).call(this);
}

view.layout = ui.extend({

    label: "Proportion Test",
    jus: "2.0",
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
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "var",
							maxItemCount: 1,
							isTarget: true,
							events: [
								{ execute: require('./prop.events').onChange_var }
							]
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
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
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "expected"
										},
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "bi",
											controls: [
												{
													type: DefaultControls.TextBox,
													typeName: 'TextBox',
													name: "ciWidth",
													suffix: "%",
													format: FormatDef.number,
													enable: "(bi)"
												},
												{
													type: DefaultControls.TextBox,
													typeName: 'TextBox',
													name: "lint",
													format: FormatDef.number,
													enable: "(bi)"
												},
												{
													type: DefaultControls.CheckBox,
													typeName: 'CheckBox',
													name: "pll",
													enable: "(bi)"
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
			margin: "large",
			cell: {"column":1,"row":1},
			stretchFactor: 1,
			horizontalAlignment: "center",
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
							verticalAlignment: "bottom"
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Expected Proportions for Alt. H",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.ListBox,
					typeName: 'ListBox',
					name: "ratio",
					showColumnHeaders: true,
					fullRowSelect: true,
					height: "large",
					stretchFactor: 1,
					events: [
						{ execute: require('./prop.events').onChange_ratio }
					],
					columns: [
						{
							name: "level",
							label: "Level",
							headerAlign: "left",
							stretchFactor: 1,
							isVirtual: true,
							maxWidth: 300,
							selectable: false,
							template:
							{
								type: DefaultControls.Label,
								typeName: 'Label'
							}							
						},
						{
							name: "ratio",
							label: "Ratio",
							maxWidth: 50,
							stretchFactor: 0.25,
							selectable: false,
							template:
							{
								type: DefaultControls.TextBox,
								typeName: 'TextBox',
								format: FormatDef.number
							}							
						},
						{
							name: "proportion",
							label: "Proportion",
							maxWidth: 100,
							headerAlign: "right",
							isVirtual: true,
							stretchFactor: 0.5,
							selectable: false,
							template:
							{
								type: DefaultControls.Label,
								typeName: 'Label',
								horizontalAlignment: "right"
							}							
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
