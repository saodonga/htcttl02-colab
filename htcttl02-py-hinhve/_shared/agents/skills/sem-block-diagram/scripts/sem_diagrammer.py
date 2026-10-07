import matplotlib.pyplot as plt
import matplotlib.patches as patches
import pandas as pd
import yaml
import argparse
import os

def draw_diagram(config_path):
    with open(config_path, 'r', encoding='utf-8') as f:
        cfg = yaml.safe_load(f)
        
    is_results = cfg.get('is_results', False)
    fig, ax = plt.subplots(figsize=tuple(cfg.get('figsize', [16, 10])))
    
    pos = {}
    nodes = cfg.get('nodes', {})
    groups = cfg.get('groups', [])
    
    # Calculate positions
    # Layer 0
    left_nodes = [k for k, v in nodes.items() if v.get('layer') == 0]
    mid_nodes = [k for k, v in nodes.items() if v.get('layer') == 1]
    right_nodes = [k for k, v in nodes.items() if v.get('layer') == 2]
    mod_nodes = [k for k, v in nodes.items() if v.get('layer') == 1.5]
    
    # Draw grouped bounding boxes for Layer 0 if specified
    for grp in groups:
        if grp.get('layer') == 0:
            x, y = grp.get('x', 0), grp.get('y', 0)
            w, h = grp.get('w', 2), grp.get('h', 3)
            ax.add_patch(patches.Rectangle((x, y), w, h, fill=False, ec='lightgray', ls='--', lw=2, zorder=0))
            ax.text(x + w/2, y + h + 0.3, grp.get('label', ''), ha='center', va='center', fontsize=12, fontweight='bold', color='gray')
            
    # Assign positions based on config or calculate automatically
    for n_id, n_data in nodes.items():
        pos[n_id] = (n_data.get('x', 0), n_data.get('y', 0))
        
    # Draw Edges
    edges = cfg.get('edges', [])
    for edge in edges:
        src = edge['src']
        tgt = edge['tgt']
        val_text = edge.get('label', '')
        status = edge.get('status', 'Chấp nhận')
        
        if src not in pos or tgt not in pos:
            continue
            
        x1, y1 = pos[src]
        x2, y2 = pos[tgt]
        
        w1 = nodes[src].get('w', 1.8)
        w2 = nodes[tgt].get('w', 1.8)
        
        start_x = x1 + w1/2
        start_y = y1
        end_x = x2 - w2/2
        end_y = y2
        
        # Adjust endpoints to not cross boxes for middle layer
        if nodes[src].get('layer') == 1 and nodes[tgt].get('layer') == 2:
            start_x = x1 + w1/2
            end_x = x2 - w2/2
            
        color = 'gray'
        style = '-'
        lw = 2
        
        if is_results:
            color = 'green' if status == "Chấp nhận" else 'red'
            style = '-' if status == "Chấp nhận" else '--'
            lw = 2.5
            
        # Mod routing
        if nodes[src].get('layer') == 1.5:
            # It's a moderator, point to the middle of the line it moderates
            mod_target = edge.get('moderates', tgt) # The node it moderates the path TO
            line_mid_x = (pos[mid_nodes[0]][0] + nodes[mid_nodes[0]].get('w',2)/2 + pos[tgt][0] - nodes[tgt].get('w',2)/2) / 2
            line_mid_y = (pos[mid_nodes[0]][1] + pos[tgt][1]) / 2
            end_x = line_mid_x
            end_y = line_mid_y
            start_y = y1 + nodes[src].get('h', 1.0)/2
            color = 'blue'
            style = '--'
            
        ax.annotate("",
                    xy=(end_x, end_y), xycoords='data',
                    xytext=(start_x, start_y), textcoords='data',
                    arrowprops=dict(arrowstyle="->", color=color, ls=style, lw=lw, shrinkA=0, shrinkB=0))
        
        if val_text and nodes[src].get('layer') != 1.5:
            mid_x = (start_x + end_x) / 2
            mid_y = (start_y + end_y) / 2
            if y2 > y1: mid_y += 0.25
            elif y2 < y1: mid_y -= 0.25
            else: mid_y += 0.25
            
            ax.text(mid_x, mid_y, val_text, color=('black' if not is_results else color), fontsize=10, fontweight='bold', ha='center', va='center', bbox=dict(facecolor='white', edgecolor='none', alpha=0.8, pad=0.5))

    # Draw Nodes
    def draw_node(x, y, text, width, height, color, lw=1.5):
        box = patches.Rectangle((x - width/2, y - height/2), width, height, fill=True, color=color, ec='black', lw=lw, zorder=2)
        ax.add_patch(box)
        ax.text(x, y, text, ha='center', va='center', fontsize=11, fontweight='bold', zorder=3)

    for node, data in nodes.items():
        draw_node(data.get('x', 0), data.get('y', 0), data.get('label', node), data.get('w', 1.8), data.get('h', 1.0), data.get('color', '#deebf7'), lw=data.get('lw', 1.5))
            
    plt.title(cfg.get('title', 'Mô Hình'), fontsize=18, fontweight='bold', pad=20)
    ax.set_xlim(cfg.get('xlim', [-1, 11]))
    ax.set_ylim(cfg.get('ylim', [-2, 9]))
    plt.axis('off')
    
    out_file = cfg.get('output', 'output.png')
    plt.savefig(out_file, bbox_inches='tight', dpi=300)
    plt.close()
    print(f"Generated {out_file}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--config', type=str, required=True, help='Path to YAML config file')
    args = parser.parse_args()
    draw_diagram(args.config)
