
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"depa","title":"First Variable","type":"Variable","required":true,"suggested":["continuous"],"permitted":["numeric"],"rejectInf":false},{"name":"depb","title":"Second Variable","type":"Variable","required":true,"suggested":["continuous"],"permitted":["numeric"],"rejectInf":false},{"name":"alt","title":"Alternative hypothesis correlation value","type":"Number","min":-1,"max":1,"default":0},{"name":"lint","type":"Number","title":"Likelihood interval support level","min":1,"max":100,"default":2},{"name":"ciWidth","title":"Likelihood-based confidence interval","type":"Number","min":50,"max":99.99,"default":95,"description":{"R":"a number between 50 and 99.9 (default: 95), width of the confidence intervals to provide\n"}},{"name":"pll","title":"Likelihood function","type":"Bool","default":false},{"name":"plt","title":"Scatterplot","type":"Bool","default":false},{"name":"line","title":"Regression Line","type":"List","options":[{"title":"None","name":"none"},{"title":"Linear","name":"linear"},{"title":"Smooth","name":"smooth"}],"default":"none","description":{"R":"`none` (default), `linear`, or `smooth`, provide respectively no regression line,  a linear regression line, or a smoothed regression line\n"}},{"name":"se","title":"Standard error","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), show the standard error for the regression line\n"}}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "LR Correlation",
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
					label: "First Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "depa",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Second Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "depb",
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
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "pll"
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "plt",
									controls: [
										{
											type: DefaultControls.LayoutBox,
											typeName: 'LayoutBox',
											controls: [
												{
													type: DefaultControls.ComboBox,
													typeName: 'ComboBox',
													name: "line",
													enable: "(plt)"
												},
												{
													type: DefaultControls.LayoutBox,
													typeName: 'LayoutBox',
													controls: [
														{
															type: DefaultControls.CheckBox,
															typeName: 'CheckBox',
															name: "se",
															enable: "(plt)"
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
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
